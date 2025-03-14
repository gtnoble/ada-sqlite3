with Ada.Strings.Wide_Unbounded;
with AUnit.Assertions;
with AUnit.Test_Caller;
with Ada_Sqlite3;
with Ada_Sqlite3.Generic_Functions;
with System.Assertions;
with Ada.Exceptions;

package body Generic_Function_Tests is
   use AUnit.Assertions;
   use Ada_Sqlite3;

   package Test_Functions is new Ada_Sqlite3.Generic_Functions
     (Context_Type => Ada.Strings.Wide_Unbounded.Unbounded_Wide_String);
   use Ada.Strings.Wide_Unbounded;
   use Test_Functions.Wide_String_Holders;

   -- Test state
   Result    : Unbounded_Wide_String := Null_Unbounded_Wide_String;
   Context   : Unbounded_Wide_String := Null_Unbounded_Wide_String;
   Frame_Sum : Integer := 0;  -- For window function state management
   
   -- Helper function to convert Wide_String to String for assertion messages
   function WS_Image (S : Wide_String) return String is
      Result : String (1 .. S'Length * 6) := (others => ' ');  -- Max 6 chars per Unicode char
      Last   : Natural := 0;
   begin
      for I in S'Range loop
         if Wide_Character'Pos (S(I)) < 128 then
            -- ASCII character
            Last := Last + 1;
            Result(Last) := Character'Val(Wide_Character'Pos(S(I)));
         else
            -- Unicode character - use hex escape
            declare
               Hex : constant String := Wide_Character'Pos(S(I))'Img;
            begin
               Last := Last + 1;
               Result(Last) := 'U';
               Last := Last + 1;
               Result(Last) := '+';
               for J in 3 .. Hex'Length loop
                  Last := Last + 1;
                  Result(Last) := Hex(J);
               end loop;
            end;
         end if;
      end loop;
      return Result(1 .. Last);
   end WS_Image;
   
   -- Callback for bounds checking tests
   function Bounds_Check_Callback 
     (Args : Test_Functions.Function_Args;
      Test_Context : Unbounded_Wide_String) return Test_Functions.Result_Type
   is
      pragma Unreferenced (Test_Context);
      Len : constant Test_Functions.Function_Args_Count := Test_Functions.Arg_Count(Args);
   begin
      -- Try to access at Length index (should raise error)
      declare
         Value : constant Integer := Test_Functions.Get_Int(Args, Test_Functions.Function_Args_Index(Len));
      begin
         return (Kind => Test_Functions.Int_Result, Int_Value => Value);
      end;
   end Bounds_Check_Callback;
   
   -- Scalar function callback
   -- Callback that returns integer for scalar tests
   function Int_Callback 
     (Args : Test_Functions.Function_Args;
      Test_Context : Unbounded_Wide_String) return Test_Functions.Result_Type
   is
      Value : constant Integer := Test_Functions.Get_Int(Args, 0);
   begin
      Result  := To_Unbounded_Wide_String(Wide_String'("Got" & Integer'Wide_Image(Value)));
      Context := Test_Context;
      return (Kind => Test_Functions.Int_Result, Int_Value => Value);
   end Int_Callback;

   -- Callback that returns UTF16 text for UTF16 tests
   function UTF16_Callback 
     (Args : Test_Functions.Function_Args;
      Test_Context : Unbounded_Wide_String) return Test_Functions.Result_Type
   is
      Value : constant Wide_String := Test_Functions.Get_Text_UTF16(Args, 0);
   begin
      Result  := To_Unbounded_Wide_String(Value);
      Context := Test_Context;
      return (Kind => Test_Functions.Text_UTF16_Result, 
              Wide_Text_Value => To_Holder(Value));
   end UTF16_Callback;
   
   -- Aggregate function callbacks
   function Aggregate_Step
     (Args : Test_Functions.Function_Args;
      Test_Context : Unbounded_Wide_String) return Test_Functions.Result_Type
   is
      Value : constant Integer := Test_Functions.Get_Int(Args, 0);
   begin
      Result  := Result & To_Unbounded_Wide_String(" +" & Integer'Wide_Image(Value));
      Context := Test_Context;
      return (Kind => Test_Functions.Int_Result, Int_Value => Value);
   end Aggregate_Step;

   function Window_Step
     (Args : Test_Functions.Function_Args;
      Test_Context : Unbounded_Wide_String) return Test_Functions.Result_Type
   is
      Value : constant Integer := Test_Functions.Get_Int(Args, 0);
   begin
      Context := Test_Context;
      Frame_Sum := Frame_Sum + Value;
      return (Kind => Test_Functions.Int_Result, Int_Value => Value);
   end Window_Step;

   function Aggregate_Final
     (Test_Context : Unbounded_Wide_String) return Test_Functions.Result_Type
   is
   begin
      Result  := Result & To_Unbounded_Wide_String(" = Sum");
      Context := Test_Context;
      return (Kind => Test_Functions.Int_Result, Int_Value => 42);
   end Aggregate_Final;

   function Window_Final
     (Test_Context : Unbounded_Wide_String) return Test_Functions.Result_Type
   is
   begin
      Context := Test_Context;
      return (Kind => Test_Functions.Int_Result, Int_Value => Frame_Sum);
   end Window_Final;

   -- Tests
   procedure Test_Scalar_Function (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database := Open (":memory:", OPEN_READWRITE or OPEN_CREATE);
      Result_Value : Integer;
   begin
      Result := Null_Unbounded_Wide_String;
      
      Test_Functions.Create_Function
        (DB      => DB,
         Name    => "test_func",
         N_Args  => 1,
         Func    => Int_Callback'Access,
         Context => Null_Unbounded_Wide_String);
      
      declare
         Stmt : Statement := Prepare(DB, "SELECT test_func(42)");
         Result : constant Result_Code := Step(Stmt);
      begin
         Assert(Result = ROW, "Expected ROW but got " & Result_Code'Image(Result));
         Result_Value := Column_Int(Stmt, 0);
      end;
      Assert (Result_Value = 42, "Expected result 42 but got" & Integer'Image(Result_Value));
      Assert (To_Wide_String(Result) = "Got 42", 
             "Expected 'Got 42' but got '" & WS_Image(To_Wide_String(Result)) & "'");
   end Test_Scalar_Function;
   
   procedure Test_Aggregate_Function (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database := Open (":memory:", OPEN_READWRITE or OPEN_CREATE);
   begin
      Result := Null_Unbounded_Wide_String;
      
      Test_Functions.Create_Aggregate
        (DB         => DB,
         Name       => "test_agg",
         N_Args     => 1,
         Step_Func  => Aggregate_Step'Access,
         Final_Func => Aggregate_Final'Access,
         Context    => Null_Unbounded_Wide_String);
      
      Execute(DB, "CREATE TABLE test (val INTEGER)");
      Execute(DB, "INSERT INTO test VALUES (1), (2), (3), (4)");
      
      declare
         Stmt : Statement := Prepare(DB, "SELECT test_agg(val) FROM test");
         Result : constant Result_Code := Step(Stmt);
      begin
         Assert(Result = ROW, "Expected ROW but got " & Result_Code'Image(Result));
         Assert(Column_Int(Stmt, 0) = 42, "Expected aggregate result 42 but got" & Integer'Image(Column_Int(Stmt, 0)));
      end;
      
      Assert (To_Wide_String(Result) = " + 1 + 2 + 3 + 4 = Sum", 
             "Expected ' + 1 + 2 + 3 + 4 = Sum' but got '" & WS_Image(To_Wide_String(Result)) & "'");
   end Test_Aggregate_Function;
   
   -- Window function state management   
   procedure Reset_Frame is
   begin
      Frame_Sum := 0;
      Result := Null_Unbounded_Wide_String;
   end Reset_Frame;

   function Window_Value 
     (Test_Context : Unbounded_Wide_String) return Test_Functions.Result_Type
   is
   begin
      Context := Test_Context;
      return (Kind => Test_Functions.Int_Result, Int_Value => Frame_Sum);
   end Window_Value;

   procedure Window_Inverse 
     (Args : Test_Functions.Function_Args;
      Test_Context : Unbounded_Wide_String)
   is
      Value : constant Integer := Test_Functions.Get_Int(Args, 0);
   begin
      Context := Test_Context;
      Frame_Sum := Frame_Sum - Value;
   end Window_Inverse;

   procedure Test_Window_Function (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database := Open (":memory:", OPEN_READWRITE or OPEN_CREATE);
   begin
      Result := Null_Unbounded_Wide_String;
      Frame_Sum := 0;  -- Initialize frame state
      
      Test_Functions.Create_Window
        (DB           => DB,
         Name         => "test_window",
         N_Args       => 1,
         Step_Func    => Window_Step'Access,
         Final_Func   => Window_Final'Access,
         Value_Func   => Window_Value'Access,
         Inverse_Func => Window_Inverse'Access,
         Context      => Null_Unbounded_Wide_String);
      
      Execute(DB, "CREATE TABLE test (val INTEGER)");
      Execute(DB, "INSERT INTO test VALUES (1), (2), (3), (4)");
      
      Reset_Frame;
      declare
         Stmt : Statement := Prepare(DB,
            "SELECT test_window(val) OVER (ORDER BY val ROWS 1 PRECEDING) FROM test ORDER BY val");
      begin
         -- First row: Only value 1
         declare
            Result_Code1 : constant Result_Code := Step(Stmt);
         begin
            Assert(Result_Code1 = ROW, "Expected ROW but got " & Result_Code'Image(Result_Code1));
            Assert(Column_Int(Stmt, 0) = 1, "Expected window function first row result 1 but got" & Integer'Image(Column_Int(Stmt, 0)));
         end;

         -- Second row: Values 1,2
         declare
            Result_Code2 : constant Result_Code := Step(Stmt);
         begin
            Assert(Result_Code2 = ROW, "Expected ROW but got " & Result_Code'Image(Result_Code2));
            Assert(Column_Int(Stmt, 0) = 3, "Expected window function second row result 3 (sum of 1+2) but got" & Integer'Image(Column_Int(Stmt, 0)));
         end;

         -- Third row: Values 2,3
         declare
            Result_Code3 : constant Result_Code := Step(Stmt);
         begin
            Assert(Result_Code3 = ROW, "Expected ROW but got " & Result_Code'Image(Result_Code3));
            Assert(Column_Int(Stmt, 0) = 5, "Expected window function third row result 5 (sum of 2+3) but got" & Integer'Image(Column_Int(Stmt, 0)));
         end;

         -- Fourth row: Values 3,4
         declare
            Result_Code4 : constant Result_Code := Step(Stmt);
         begin
            Assert(Result_Code4 = ROW, "Expected ROW but got " & Result_Code'Image(Result_Code4));
            Assert(Column_Int(Stmt, 0) = 7, "Expected window function fourth row result 7 (sum of 3+4) but got" & Integer'Image(Column_Int(Stmt, 0)));
         end;

         declare
            Final_Result : constant Result_Code := Step(Stmt);
         begin
            Assert(Final_Result = DONE, "Expected DONE but got " & Result_Code'Image(Final_Result));
         end;
      end;
   end Test_Window_Function;
   
   procedure Test_UTF16_Text (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database := Open (":memory:", OPEN_READWRITE or OPEN_CREATE);
   begin
      Result := Null_Unbounded_Wide_String;
      
      Test_Functions.Create_Function
        (DB      => DB,
         Name    => "test_utf16",
         N_Args  => 1,
         Func    => UTF16_Callback'Access,
         Context => Null_Unbounded_Wide_String);
      
         declare
            Wide_Text : constant Wide_String := "世界";
            Stmt : Statement := Prepare(DB, "SELECT test_utf16(?)");
            Result : Result_Code;
         begin
            Bind_Text_UTF16(Stmt, 1, Wide_Text);
            Result := Step(Stmt);
            Assert(Result = ROW, "Expected ROW but got " & Result_Code'Image(Result));
         Assert(Column_Text_UTF16(Stmt, 0) = Wide_Text, 
               "Expected UTF16 text '" & WS_Image(Wide_Text) & "' but got '" & 
               WS_Image(Column_Text_UTF16(Stmt, 0)) & "'");
      end;
      Assert (To_Wide_String(Result) = "世界", 
             "Expected UTF-16 text '" & WS_Image("世界") & "' but got '" & 
             WS_Image(To_Wide_String(Result)) & "'");
   end Test_UTF16_Text;
   
   procedure Test_Context_Cleanup (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database := Open (":memory:", OPEN_READWRITE or OPEN_CREATE);
      Test_Context : constant Unbounded_Wide_String := 
         To_Unbounded_Wide_String("test context");
   begin
      Result := Null_Unbounded_Wide_String;
      
      Test_Functions.Create_Function
        (DB      => DB,
         Name    => "test_cleanup",
         N_Args  => 1,
         Func    => Int_Callback'Access,
         Context => Test_Context);
      
         declare
            Stmt : Statement := Prepare(DB, "SELECT test_cleanup(1)");
            Result : constant Result_Code := Step(Stmt);
         begin
            Assert(Result = ROW, "Expected ROW but got " & Result_Code'Image(Result));
      end;
      
      Assert (To_Wide_String(Context) = "test context", 
             "Expected context 'test context' but got '" & WS_Image(To_Wide_String(Context)) & "'");
   end Test_Context_Cleanup;
   
   procedure Test_Out_Of_Bounds_Access (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database := Open (":memory:", OPEN_READWRITE or OPEN_CREATE);
   begin
      Test_Functions.Create_Function
        (DB      => DB,
         Name    => "bounds_test",
         N_Args  => 1,
         Func    => Bounds_Check_Callback'Access,
         Context => Null_Unbounded_Wide_String);
      
      -- Test with one argument - should raise Constraint_Error
      begin
         declare
            Stmt : Statement := Prepare(DB, "SELECT bounds_test(42)");
            Result : constant Result_Code := Step(Stmt);
            pragma Unreferenced (Result);
         begin
            Assert(False, "Expected Constraint_Error for out of bounds access");
         end;
      exception
         when E : Constraint_Error =>
            -- Verify error message contains useful information
         null;
      end;
   end Test_Out_Of_Bounds_Access;
   
   procedure Test_Empty_Args (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database := Open (":memory:", OPEN_READWRITE or OPEN_CREATE);
   begin
      Result := Null_Unbounded_Wide_String;
      
      Test_Functions.Create_Function
        (DB      => DB,
         Name    => "empty_args",
         N_Args  => 0,
         Func    => Int_Callback'Access,
         Context => Null_Unbounded_Wide_String);
      
      begin
         declare
            Stmt : Statement := Prepare(DB, "SELECT empty_args()");
            Result : constant Result_Code := Step(Stmt);
            pragma Unreferenced(Result);
         begin
            -- If we get here, the test should fail
            Assert (False, "Expected assertion error for accessing empty args");
         end;
      exception
         when Constraint_Error =>
            -- Test passes - expected assertion error
            null;
      end;
   end Test_Empty_Args;
   
   procedure Test_Multiple_Functions (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database := Open (":memory:", OPEN_READWRITE or OPEN_CREATE);
      Value1, Value2 : Integer;
   begin
      Result := Null_Unbounded_Wide_String;
      
      -- Register multiple functions
      Test_Functions.Create_Function
        (DB      => DB,
         Name    => "func1",
         N_Args  => 1,
         Func    => Int_Callback'Access,
         Context => Null_Unbounded_Wide_String);
         
      Test_Functions.Create_Function
        (DB      => DB,
         Name    => "func2",
         N_Args  => 1,
         Func    => Int_Callback'Access,
         Context => Null_Unbounded_Wide_String);
      
         declare
            Stmt : Statement := Prepare(DB, "SELECT func1(1), func2(2)");
            Result : constant Result_Code := Step(Stmt);
         begin
            Assert(Result = ROW, "Expected ROW but got " & Result_Code'Image(Result));
            Value1 := Column_Int(Stmt, 0);
            Value2 := Column_Int(Stmt, 1);
         end;
      
      Assert (Value1 = 1, "Expected first function result 1 but got" & Integer'Image(Value1));
      Assert (Value2 = 2, "Expected second function result 2 but got" & Integer'Image(Value2));
      Assert (To_Wide_String(Result) = "Got 2", 
             "Expected callback result 'Got 2' but got '" & WS_Image(To_Wide_String(Result)) & "'");
   end Test_Multiple_Functions;
   
   -- Register test routines to call
   package Caller is new AUnit.Test_Caller (Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite := 
         new AUnit.Test_Suites.Test_Suite;
   begin
      Result.Add_Test (Caller.Create ("Test_Scalar_Function", Test_Scalar_Function'Access));
      Result.Add_Test (Caller.Create ("Test_Aggregate_Function", Test_Aggregate_Function'Access));
      Result.Add_Test (Caller.Create ("Test_Window_Function", Test_Window_Function'Access));
      Result.Add_Test (Caller.Create ("Test_UTF16_Text", Test_UTF16_Text'Access));
      Result.Add_Test (Caller.Create ("Test_Context_Cleanup", Test_Context_Cleanup'Access));
      Result.Add_Test (Caller.Create ("Test_Empty_Args", Test_Empty_Args'Access));
      Result.Add_Test (Caller.Create ("Test_Out_Of_Bounds_Access", Test_Out_Of_Bounds_Access'Access));
      Result.Add_Test (Caller.Create ("Test_Multiple_Functions", Test_Multiple_Functions'Access));
      return Result;
   end Suite;

end Generic_Function_Tests;
