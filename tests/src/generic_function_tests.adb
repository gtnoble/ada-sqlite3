with Ada.Strings.Wide_Unbounded;
with AUnit.Assertions;
with AUnit.Test_Caller;
with Ada_Sqlite3;
with Ada_Sqlite3.Generic_Functions;

package body Generic_Function_Tests is
   use AUnit.Assertions;
   use Ada_Sqlite3;  -- Add this to make database operations visible

   package Test_Functions is new Ada_Sqlite3.Generic_Functions
     (Context_Type => Ada.Strings.Wide_Unbounded.Unbounded_Wide_String);
   use Ada.Strings.Wide_Unbounded;
   use Test_Functions.Wide_String_Holders;

   -- Test state
   Result  : Unbounded_Wide_String := Null_Unbounded_Wide_String;
   Context : Unbounded_Wide_String := Null_Unbounded_Wide_String;
   
   -- Callback for bounds checking tests
   function Bounds_Check_Callback 
     (Args : Test_Functions.Function_Args;
      Test_Context : Unbounded_Wide_String) return Test_Functions.Result_Type
   is
      Len : constant Natural := Test_Functions.Arg_Count(Args);
   begin
      -- Try to access at Length index (should raise error)
      declare
         Value : constant Integer := Test_Functions.Get_Int(Args, Len);
         pragma Unreferenced (Value);
      begin
         return (Kind => Test_Functions.Int_Result, Int_Value => 0);
      end;
   exception
      when Constraint_Error =>
         -- Expected error, return length as confirmation
         return (Kind => Test_Functions.Int_Result, Int_Value => Len);
   end Bounds_Check_Callback;
   
   -- Scalar function callback
   -- Callback that returns integer for scalar tests
   function Int_Callback 
     (Args : Test_Functions.Function_Args;
      Test_Context : Unbounded_Wide_String) return Test_Functions.Result_Type
   is
      Value : constant Integer := Test_Functions.Get_Int(Args, 0);
   begin
      Result  := To_Unbounded_Wide_String(Wide_String'("Got " & Integer'Wide_Image(Value)));
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
      Result  := Result & To_Unbounded_Wide_String(" + " & Integer'Wide_Image(Value));
      Context := Test_Context;
      return (Kind => Test_Functions.Int_Result, Int_Value => Value);
   end Aggregate_Step;
   
   function Aggregate_Final
     (Test_Context : Unbounded_Wide_String) return Test_Functions.Result_Type
   is
   begin
      Result  := Result & To_Unbounded_Wide_String(" = Sum");
      Context := Test_Context;
      return (Kind => Test_Functions.Int_Result, Int_Value => 42);
   end Aggregate_Final;

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
      begin
         Assert(Step(Stmt) = ROW, "Expected a row result");
         Result_Value := Column_Int(Stmt, 0);
      end;
      Assert (Result_Value = 42, "Function should return 42");
      Assert (To_Wide_String(Result) = "Got 42",
             "Unexpected scalar function result");
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
      Execute(DB, "INSERT INTO test VALUES (1), (2), (3)");
      
      declare
         Stmt : Statement := Prepare(DB, "SELECT test_agg(val) FROM test");
      begin
         Assert(Step(Stmt) = ROW, "Expected a row result");
         Assert(Column_Int(Stmt, 0) = 42, "Aggregate should return 42");
      end;
      
      Assert (To_Wide_String(Result) = " + 1 + 2 + 3 = Sum",
              "Unexpected aggregate function result");
   end Test_Aggregate_Function;
   
   -- Window function callbacks
   procedure Window_Inverse
     (Args : Test_Functions.Function_Args;
      Test_Context : Unbounded_Wide_String)
   is
      Value : constant Integer := Test_Functions.Get_Int(Args, 0);
   begin
      Result  := Result & To_Unbounded_Wide_String(" - " & Integer'Wide_Image(Value));
      Context := Test_Context;
   end Window_Inverse;

   procedure Test_Window_Function (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database := Open (":memory:", OPEN_READWRITE or OPEN_CREATE);
   begin
      Result := Null_Unbounded_Wide_String;
      
      Test_Functions.Create_Window
        (DB           => DB,
         Name         => "test_window",
         N_Args       => 1,
         Step_Func    => Aggregate_Step'Access,
         Final_Func   => Aggregate_Final'Access,
         Value_Func   => Aggregate_Final'Access,
         Inverse_Func => Window_Inverse'Access,
         Context      => Null_Unbounded_Wide_String);
      
      Execute(DB, "CREATE TABLE test (val INTEGER)");
      Execute(DB, "INSERT INTO test VALUES (1), (2), (3)");
      
      declare
         Stmt : Statement := Prepare(DB, "SELECT test_window(val) OVER (ORDER BY val) FROM test");
      begin
         Assert(Step(Stmt) = ROW, "Expected a row result");
         Assert(Column_Int(Stmt, 0) = 42, "Window function should return 42");
      end;
      
      Assert (To_Wide_String(Result) = " + 1 + 2 + 3 = Sum",
              "Unexpected window function result");
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
      begin
         Bind_Text_UTF16(Stmt, 1, Wide_Text);
         Assert(Step(Stmt) = ROW, "Expected a row result");
         Assert(Column_Text_UTF16(Stmt, 0) = Wide_Text,
               "UTF16 function should return correct text");
      end;
      Assert (To_Wide_String(Result) = "世界",
             "Unexpected UTF-16 text result");
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
      begin
         Assert(Step(Stmt) = ROW, "Expected a row result");
      end;
      
      Assert (To_Wide_String(Context) = "test context",
             "Context not properly passed to callback");
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
      
      -- Test with one argument
      declare
         Stmt : Statement := Prepare(DB, "SELECT bounds_test(42)");
      begin
         Assert(Step(Stmt) = ROW, "Expected a row result");
         Assert(Column_Int(Stmt, 0) = 1, 
                "Function should return argument count as proof of bounds check");
      end;
      
      -- Test with no arguments
      declare
         Stmt : Statement := Prepare(DB, "SELECT bounds_test()");
      begin
         Assert(Step(Stmt) = ROW, "Expected a row result");
         Assert(Column_Int(Stmt, 0) = 0, 
                "Function should return 0 for empty args");
      end;
   end Test_Out_Of_Bounds_Access;
   
   procedure Test_Empty_Args (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database := Open (":memory:", OPEN_READWRITE or OPEN_CREATE);
   begin
      Result := Null_Unbounded_Wide_String;
      
      -- Create a function that expects zero arguments
      Test_Functions.Create_Function
        (DB      => DB,
         Name    => "empty_args",
         N_Args  => 0,
         Func    => Int_Callback'Access,
         Context => Null_Unbounded_Wide_String);
      
      declare
         Stmt : Statement := Prepare(DB, "SELECT empty_args()");
      begin
         Assert(Step(Stmt) = ROW, "Expected a row result");
         Assert(Column_Int(Stmt, 0) = 42, 
                "Function should return default value for empty args");
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
      begin
         Assert(Step(Stmt) = ROW, "Expected a row result");
         Value1 := Column_Int(Stmt, 0);
         Value2 := Column_Int(Stmt, 1);
      end;
      
      Assert (Value1 = 1, "First function should return 1");
      Assert (Value2 = 2, "Second function should return 2");
      Assert (To_Wide_String(Result) = "Got 2",
             "Callback result should show last function call");
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
