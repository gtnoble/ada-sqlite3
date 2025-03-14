-------------------------------------------------------------------------------
-- Statement Tests for Ada_Sqlite3
--
-- Copyright (c) 2025 Garret Noble
--
-- Licensed under the terms of the MIT License or Apache-2.0 with LLVM exception
-------------------------------------------------------------------------------

with AUnit.Assertions;
with AUnit.Test_Caller;
with Ada_Sqlite3;

package body Statement_Tests is

   use AUnit.Assertions;
   use Ada_Sqlite3;

   --  Setup test database
   function Setup_Test_DB return Database is
   begin
      return DB : Database := Open (":memory:", OPEN_READWRITE or OPEN_CREATE) do
      --  Create a test table
         Execute (DB, "CREATE TABLE test (id INTEGER PRIMARY KEY, int_val INTEGER, real_val REAL, text_val TEXT, null_val NULL)");
         
         --  Insert some test data
         Execute (DB, "INSERT INTO test (int_val, real_val, text_val, null_val) VALUES (42, 3.14, 'hello', NULL)");
         Execute (DB, "INSERT INTO test (int_val, real_val, text_val, null_val) VALUES (123, 2.71, 'world', NULL)");
      end return;
   end Setup_Test_DB;

   --  Test preparing and finalizing a statement
   procedure Test_Prepare_Finalize (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database := Setup_Test_DB;
   begin
      
      --  Create a nested scope to test statement finalization before database
      declare
         Nested_Stmt : Statement := Prepare (DB, "SELECT COUNT(*) FROM test");
      begin
         --  Use the statement
         Step (Nested_Stmt);
         --  Nested_Stmt will be finalized here when it goes out of scope
      end;
      
      
      --  Let the statement be finalized automatically when DB is finalized
   end Test_Prepare_Finalize;

   --  Test resetting and clearing bindings
   procedure Test_Reset_Clear_Bindings (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database := Setup_Test_DB;
      --  Prepare a parameterized statement
      Stmt : Statement := Prepare (DB, "SELECT * FROM test WHERE int_val > ?");
      Result : Result_Code;
      Count : Integer;
      Binding_Error : Boolean;
   begin
      
      --  Bind a parameter and execute
      Bind_Int (Stmt, 1, 0);
      
      --  Count rows
      Count := 0;
      loop
         Result := Step (Stmt);
         exit when Result = DONE;
         
         if Result = ROW then
            Count := Count + 1;
         end if;
      end loop;
      
      --  Should have 2 rows
      Assert (Count = 2, 
              "Expected 2 rows with int_val > 0 but got" & Integer'Image(Count));
      
      --  Reset the statement
      Reset (Stmt);
      
      --  Bind a different parameter and execute again
      Bind_Int (Stmt, 1, 100);
      
      --  Count rows
      Count := 0;
      loop
         Result := Step (Stmt);
         exit when Result = DONE;
         
         if Result = ROW then
            Count := Count + 1;
         end if;
      end loop;
      
      --  Should have 1 row
      Assert (Count = 1, 
              "Expected 1 row with int_val > 100 but got" & Integer'Image(Count));
      
   end Test_Reset_Clear_Bindings;

   --  Test stepping through results
   procedure Test_Step (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database := Setup_Test_DB;
      Stmt : Statement := Prepare (DB, "SELECT * FROM test ORDER BY id");
      Result : Result_Code;
      Count : Integer;
   begin
      --  Prepare a statement
      
      --  Step through results
      Count := 0;
      loop
         Result := Step (Stmt);
         exit when Result = DONE;
         
         if Result = ROW then
            Count := Count + 1;
            
            --  Check row values
            if Count = 1 then
               Assert (Column_Int (Stmt, 1) = 42, 
                       "Expected first row int_val 42 but got" & Integer'Image(Column_Int (Stmt, 1)));
               Assert (abs (Column_Double (Stmt, 2) - 3.14) < 0.001, 
                       "Expected first row real_val 3.14 but got" & Float'Image(Float(Column_Double (Stmt, 2))));
               Assert (Column_Text (Stmt, 3) = "hello", 
                       "Expected first row text_val 'hello' but got '" & Column_Text (Stmt, 3) & "'");
            elsif Count = 2 then
               Assert (Column_Int (Stmt, 1) = 123, 
                       "Expected second row int_val 123 but got" & Integer'Image(Column_Int (Stmt, 1)));
               Assert (abs (Column_Double (Stmt, 2) - 2.71) < 0.001, 
                       "Expected second row real_val 2.71 but got" & Float'Image(Float(Column_Double (Stmt, 2))));
               Assert (Column_Text (Stmt, 3) = "world", 
                       "Expected second row text_val 'world' but got '" & Column_Text (Stmt, 3) & "'");
            end if;
         end if;
      end loop;
      
      --  Should have 2 rows
      Assert (Count = 2, "Expected 2 rows but got" & Integer'Image(Count));
      
      --  Clean up
      
   end Test_Step;

   --  Test binding NULL values
   procedure Test_Bind_Null (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database := Setup_Test_DB;
      Stmt1 : Statement := Prepare (DB, "INSERT INTO test (int_val, real_val, text_val, null_val) VALUES (?, ?, ?, ?)");
      Stmt2 : Statement := Prepare (DB, "SELECT * FROM test WHERE int_val = 999");
      Result : Result_Code;
   begin
      
      --  Bind parameters with NULL
      Bind_Int (Stmt1, 1, 999);
      Bind_Double (Stmt1, 2, 9.99);
      Bind_Null (Stmt1, 3);
      Bind_Null (Stmt1, 4);
      
      --  Execute
      Step (Stmt1);
      
      --  Verify the inserted row
      Result := Step (Stmt2);
      
      --  Should have a row
      Assert (Result = ROW, "Expected ROW but got " & Result_Code'Image(Result) & " for row with int_val = 999");
      
      --  Check NULL values
      Assert (Column_Is_Null (Stmt2, 3), "Expected text_val to be NULL but it was not");
      Assert (Column_Is_Null (Stmt2, 4), "Expected null_val to be NULL but it was not");
      
      --  Clean up
      
   end Test_Bind_Null;

   --  Test binding integer values
   procedure Test_Bind_Int (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database := Setup_Test_DB;
      Stmt1 : Statement := Prepare (DB, "INSERT INTO test (int_val) VALUES (?)");
      Stmt2 : Statement := Prepare (DB, "SELECT int_val FROM test WHERE int_val = 12345");
      Result : Result_Code;
   begin
      --  Prepare an insert statement
      
      --  Bind integer parameter
      Bind_Int (Stmt1, 1, 12345);
      
      --  Execute
      Step (Stmt1);
      
      --  Verify the inserted row
      Result := Step (Stmt2);
      
      --  Should have a row
      Assert (Result = ROW, "Expected ROW but got " & Result_Code'Image(Result) & " for row with int_val = 12345");
      Assert (Column_Int (Stmt2, 0) = 12345, 
              "Expected int_val 12345 but got" & Integer'Image(Column_Int (Stmt2, 0)));
      
      --  Clean up
      
   end Test_Bind_Int;

   --  Test binding 64-bit integer values
      procedure Test_Bind_Int64 (T : in out Test) is
         pragma Unreferenced (T);
         DB : Database := Setup_Test_DB;
         Stmt1 : Statement := Prepare (DB, "INSERT INTO test (int_val) VALUES (?)");
         Stmt2 : Statement := Prepare (DB, "SELECT int_val FROM test WHERE int_val = ?");
         Result : Result_Code;
         Big_Value : constant Long_Integer := 9223372036854775807;
      begin
      
         --  Prepare an insert statement
      
         --  Bind 64-bit integer parameter
         Bind_Int64 (Stmt1, 1, Big_Value);
      
         --  Execute
         Step (Stmt1);
      
         --  Verify the inserted row
         Bind_Int64 (Stmt2, 1, Big_Value);
         Result := Step (Stmt2);
      
      --  Should have a row
      Assert (Result = ROW, "Expected ROW but got " & Result_Code'Image(Result) & " for row with int_val = " & Big_Value'Image);
         Assert (Column_Int64 (Stmt2, 0) = Big_Value, 
                 "Expected int_val" & Big_Value'Image & " but got" & Long_Integer'Image(Column_Int64 (Stmt2, 0)));
      
         --  Clean up
      
      end Test_Bind_Int64;

   --  Test binding floating-point values
      procedure Test_Bind_Double (T : in out Test) is
         pragma Unreferenced (T);
         DB : Database := Setup_Test_DB;
         Stmt1 : Statement := Prepare (DB, "INSERT INTO test (real_val) VALUES (?)");
         Stmt2 : Statement := Prepare (DB, "SELECT real_val FROM test WHERE abs(real_val - ?) < 0.0001");
         Result : Result_Code;
         Pi : constant Float := 3.14159265359;
      begin
      
         --  Prepare an insert statement
      
         --  Bind double parameter
         Bind_Double (Stmt1, 1, Pi);
      
         --  Execute
         Step (Stmt1);
      
         --  Verify the inserted row
         Bind_Double (Stmt2, 1, Pi);
         Result := Step (Stmt2);
      
      --  Should have a row
      Assert (Result = ROW, "Expected ROW but got " & Result_Code'Image(Result) & " for row with real_val = " & Pi'Image);
         Assert (abs (Column_Double (Stmt2, 0) - Pi) < 0.0001, 
                 "Expected real_val" & Pi'Image & " but got" & Float'Image(Float(Column_Double (Stmt2, 0))));
      
         --  Clean up
      
      end Test_Bind_Double;

   --  Test binding text values
      procedure Test_Bind_Text (T : in out Test) is
         pragma Unreferenced (T);
         DB : Database := Setup_Test_DB;
         Stmt2 : Statement := Prepare (DB, "SELECT text_val FROM test WHERE text_val = ?");
         Result : Result_Code;
         Text_Value : constant String := "This is a test string with special chars: !@#$%^&*()";
      begin
      
         --  Insert a row with text value using Execute
         Execute (DB, "INSERT INTO test (text_val) VALUES ('" & Text_Value & "')");
      
         --  Verify the inserted row using a statement
         Bind_Text (Stmt2, 1, Text_Value);
         Result := Step (Stmt2);
      
      --  Should have a row
      Assert (Result = ROW, "Expected ROW but got " & Result_Code'Image(Result) & " for row with text_val = '" & Text_Value & "'");
         Assert (Column_Text (Stmt2, 0) = Text_Value, 
                 "Expected text_val '" & Text_Value & "' but got '" & Column_Text (Stmt2, 0) & "'");
      
      end Test_Bind_Text;

   --  Test binding parameters by name
   procedure Test_Bind_Parameter_Index (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database := Setup_Test_DB;
      Stmt : Statement := Prepare (DB, "SELECT * FROM test WHERE int_val = :value");
      Result : Result_Code;
      Param_Index : Positive;
   begin
      
      --  Prepare a statement with named parameters
      
      --  Get parameter index
      Bind_Parameter_Index (Stmt, ":value", Param_Index);
      
      --  Bind parameter
      Bind_Int (Stmt, Param_Index, 42);
      
      --  Execute
      Result := Step (Stmt);
      
      --  Should have a row
      Assert (Result = ROW, "Expected ROW but got " & Result_Code'Image(Result) & " for row with int_val = 42");
      Assert (Column_Int (Stmt, 1) = 42, 
              "Expected int_val 42 but got" & Integer'Image(Column_Int (Stmt, 1)));
      
      --  Clean up
      
   end Test_Bind_Parameter_Index;

   --  Test getting column count
procedure Test_Column_Count (T : in out Test) is
   pragma Unreferenced (T);
   DB : Database := Setup_Test_DB;
   Stmt1 : constant Statement := Prepare (DB, "SELECT * FROM test");
   Stmt2 : constant Statement := Prepare (DB, "SELECT int_val, text_val FROM test");
begin
   
   --  Check column count
   Assert (Column_Count (Stmt1) = 5, 
           "Expected 5 columns but got" & Integer'Image(Column_Count (Stmt1)));
   
   --  Check column count
   Assert (Column_Count (Stmt2) = 2, 
           "Expected 2 columns but got" & Integer'Image(Column_Count (Stmt2)));
   
end Test_Column_Count;

   --  Test getting column names
procedure Test_Column_Name (T : in out Test) is
   pragma Unreferenced (T);
   DB : Database := Setup_Test_DB;
   Stmt : constant Statement := Prepare (DB, "SELECT id, int_val, real_val, text_val, null_val FROM test");
begin
   
   --  Prepare a statement
   
   --  Check column names
   Assert (Column_Name (Stmt, 0) = "id", 
           "Expected column 0 name 'id' but got '" & Column_Name (Stmt, 0) & "'");
   Assert (Column_Name (Stmt, 1) = "int_val", 
           "Expected column 1 name 'int_val' but got '" & Column_Name (Stmt, 1) & "'");
   Assert (Column_Name (Stmt, 2) = "real_val", 
           "Expected column 2 name 'real_val' but got '" & Column_Name (Stmt, 2) & "'");
   Assert (Column_Name (Stmt, 3) = "text_val", 
           "Expected column 3 name 'text_val' but got '" & Column_Name (Stmt, 3) & "'");
   Assert (Column_Name (Stmt, 4) = "null_val", 
           "Expected column 4 name 'null_val' but got '" & Column_Name (Stmt, 4) & "'");
   
end Test_Column_Name;

   --  Test getting column types
procedure Test_Column_Type (T : in out Test) is
   pragma Unreferenced (T);
   DB : Database := Setup_Test_DB;
   Stmt : Statement := Prepare (DB, "SELECT id, int_val, real_val, text_val, null_val FROM test LIMIT 1");
   Result : Result_Code;
begin
   
   --  Prepare a statement
   
   --  Execute
   Result := Step (Stmt);
   Assert (Result = ROW, "Expected ROW but got " & Result_Code'Image(Result));
   
   --  Check column types
   Assert (Get_Column_Type (Stmt, 0) = Integer_Type, 
           "Expected column 0 type Integer_Type but got " & Column_Type'Image(Get_Column_Type (Stmt, 0)));
   Assert (Get_Column_Type (Stmt, 1) = Integer_Type, 
           "Expected column 1 type Integer_Type but got " & Column_Type'Image(Get_Column_Type (Stmt, 1)));
   Assert (Get_Column_Type (Stmt, 2) = Float_Type, 
           "Expected column 2 type Float_Type but got " & Column_Type'Image(Get_Column_Type (Stmt, 2)));
   Assert (Get_Column_Type (Stmt, 3) = Text_Type, 
           "Expected column 3 type Text_Type but got " & Column_Type'Image(Get_Column_Type (Stmt, 3)));
   Assert (Get_Column_Type (Stmt, 4) = Null_Type, 
           "Expected column 4 type Null_Type but got " & Column_Type'Image(Get_Column_Type (Stmt, 4)));
   
   --  Clean up
   
   
end Test_Column_Type;

   --  Test getting integer column values
procedure Test_Column_Int (T : in out Test) is
   pragma Unreferenced (T);
   DB : Database := Setup_Test_DB;
   Stmt : Statement := Prepare (DB, "SELECT int_val FROM test WHERE int_val = 42");
   Result : Result_Code;
begin
   
   --  Prepare a statement
   
   --  Execute
   Result := Step (Stmt);
   Assert (Result = ROW, "Expected ROW but got " & Result_Code'Image(Result));
   
   --  Check column value
   Assert (Column_Int (Stmt, 0) = 42, 
           "Expected int_val 42 but got" & Integer'Image(Column_Int (Stmt, 0)));
   
end Test_Column_Int;

   --  Test getting 64-bit integer column values
procedure Test_Column_Int64 (T : in out Test) is
   pragma Unreferenced (T);
   DB : Database := Setup_Test_DB;
   Big_Value : constant Long_Integer := 9223372036854775807;
   Stmt : Statement := Prepare (DB, "SELECT int_val FROM test WHERE int_val = " & Big_Value'Image);
   Result : Result_Code;
begin
   
   --  Insert a row with a big integer
   Execute (DB, "INSERT INTO test (int_val) VALUES (" & Big_Value'Image & ")");
   
   --  Execute
   Result := Step (Stmt);
   Assert (Result = ROW, "Should have a row");
   
   --  Check column value
   Assert (Column_Int64 (Stmt, 0) = Big_Value, 
           "Expected int_val" & Big_Value'Image & " but got" & Long_Integer'Image(Column_Int64 (Stmt, 0)));
   
   --  Clean up
   
   
end Test_Column_Int64;

   --  Test getting floating-point column values
procedure Test_Column_Double (T : in out Test) is
   pragma Unreferenced (T);
   DB : Database := Setup_Test_DB;
   Stmt : Statement := Prepare (DB, "SELECT real_val FROM test WHERE abs(real_val - 3.14) < 0.01");
   Result : Result_Code;
begin
   
   --  Prepare a statement
   
   --  Execute
   Result := Step (Stmt);
   Assert (Result = ROW, "Should have a row");
   
   --  Check column value
   Assert (abs (Column_Double (Stmt, 0) - 3.14) < 0.01, 
           "Expected real_val approximately 3.14 but got" & Float'Image(Float(Column_Double (Stmt, 0))));
   
   --  Clean up
   
   
end Test_Column_Double;

   --  Test getting text column values
procedure Test_Column_Text (T : in out Test) is
   pragma Unreferenced (T);
   DB : Database := Setup_Test_DB;
   Stmt : Statement := Prepare (DB, "SELECT text_val FROM test WHERE text_val = 'hello'");
   Result : Result_Code;
begin
   
   --  Prepare a statement
   
   --  Execute
   Result := Step (Stmt);
   Assert (Result = ROW, "Should have a row");
   
   --  Check column value
   Assert (Column_Text (Stmt, 0) = "hello", 
           "Expected text_val 'hello' but got '" & Column_Text (Stmt, 0) & "'");
   
end Test_Column_Text;

   --  Test checking for NULL column values
procedure Test_Column_Is_Null (T : in out Test) is
   pragma Unreferenced (T);
   DB : Database := Setup_Test_DB;
   Stmt : Statement := Prepare (DB, "SELECT null_val FROM test LIMIT 1");
   Result : Result_Code;
begin
   
   --  Prepare a statement
   
   --  Execute
   Result := Step (Stmt);
   Assert (Result = ROW, "Should have a row");
   
   --  Check column value
   Assert (Column_Is_Null (Stmt, 0), "Expected null_val to be NULL but it was not");
   
end Test_Column_Is_Null;

   --  Test invalid statement operations
   procedure Test_Invalid_Statement (T : in out Test) is
      pragma Unreferenced (T);
      DB: Database := Setup_Test_DB;
      Exception_Raised : Boolean;
   begin
      
      --  Try to prepare an invalid SQL statement
      Exception_Raised := False;
      declare
      begin
         declare
            Stmt : Statement := Prepare (DB, "SELECT * FROM nonexistent_table");
         begin
            Step (Stmt);
         end;
      exception
         when SQLite_Error =>
            Exception_Raised := True;
      end;

      Assert (Exception_Raised, 
              "Expected SQLite_Error exception for invalid SQL but no exception was raised");
      
   end Test_Invalid_Statement;

   --  Test statement finalization order
   procedure Test_Statement_Finalization_Order (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database := Setup_Test_DB;
      Stmt1 : Statement := Prepare (DB, "SELECT * FROM test");
      Stmt2 : Statement := Prepare (DB, "SELECT COUNT(*) FROM test");
      Stmt3 : Statement := Prepare (DB, "SELECT int_val FROM test WHERE int_val = 42");
   begin
      
      --  Use the statements
      Step (Stmt1);
      Step (Stmt2);
      Step (Stmt3);
      
      --  The statements and database will be finalized in the correct order
      --  when they go out of scope at the end of this procedure
   end Test_Statement_Finalization_Order;

   --  Register test routines to call
   package Caller is new AUnit.Test_Caller (Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite := 
         new AUnit.Test_Suites.Test_Suite;
   begin
      Result.Add_Test (Caller.Create ("Test_Prepare_Finalize", Test_Prepare_Finalize'Access));
      Result.Add_Test (Caller.Create ("Test_Statement_Finalization_Order", Test_Statement_Finalization_Order'Access));
      Result.Add_Test (Caller.Create ("Test_Reset_Clear_Bindings", Test_Reset_Clear_Bindings'Access));
      Result.Add_Test (Caller.Create ("Test_Step", Test_Step'Access));
      Result.Add_Test (Caller.Create ("Test_Bind_Null", Test_Bind_Null'Access));
      Result.Add_Test (Caller.Create ("Test_Bind_Int", Test_Bind_Int'Access));
      Result.Add_Test (Caller.Create ("Test_Bind_Int64", Test_Bind_Int64'Access));
      Result.Add_Test (Caller.Create ("Test_Bind_Double", Test_Bind_Double'Access));
      Result.Add_Test (Caller.Create ("Test_Bind_Text", Test_Bind_Text'Access));
      Result.Add_Test (Caller.Create ("Test_Bind_Parameter_Index", Test_Bind_Parameter_Index'Access));
      Result.Add_Test (Caller.Create ("Test_Column_Count", Test_Column_Count'Access));
      Result.Add_Test (Caller.Create ("Test_Column_Name", Test_Column_Name'Access));
      Result.Add_Test (Caller.Create ("Test_Column_Type", Test_Column_Type'Access));
      Result.Add_Test (Caller.Create ("Test_Column_Int", Test_Column_Int'Access));
      Result.Add_Test (Caller.Create ("Test_Column_Int64", Test_Column_Int64'Access));
      Result.Add_Test (Caller.Create ("Test_Column_Double", Test_Column_Double'Access));
      Result.Add_Test (Caller.Create ("Test_Column_Text", Test_Column_Text'Access));
      Result.Add_Test (Caller.Create ("Test_Column_Is_Null", Test_Column_Is_Null'Access));
      Result.Add_Test (Caller.Create ("Test_Invalid_Statement", Test_Invalid_Statement'Access));
      
      return Result;
   end Suite;

end Statement_Tests;
