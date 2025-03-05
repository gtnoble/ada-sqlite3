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
   procedure Setup_Test_DB (DB : in out Database) is
   begin
      --  Open an in-memory database
      Open (DB, ":memory:", OPEN_READWRITE or OPEN_CREATE);
      
      --  Create a test table
      Execute (DB, "CREATE TABLE test (id INTEGER PRIMARY KEY, int_val INTEGER, real_val REAL, text_val TEXT, null_val NULL)");
      
      --  Insert some test data
      Execute (DB, "INSERT INTO test (int_val, real_val, text_val, null_val) VALUES (42, 3.14, 'hello', NULL)");
      Execute (DB, "INSERT INTO test (int_val, real_val, text_val, null_val) VALUES (123, 2.71, 'world', NULL)");
   end Setup_Test_DB;

   --  Test preparing and finalizing a statement
   procedure Test_Prepare_Finalize (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database;
      Stmt : Statement;
   begin
      Setup_Test_DB (DB);
      
      --  Test preparing a statement
      Prepare (Stmt, DB, "SELECT * FROM test");
      
      --  Finalize the statement
      Finalize_Statement (Stmt);
      
      --  Prepare another statement
      Prepare (Stmt, DB, "SELECT COUNT(*) FROM test");
      
      --  Let the statement be finalized automatically
      Close (DB);
   end Test_Prepare_Finalize;

   --  Test resetting and clearing bindings
   procedure Test_Reset_Clear_Bindings (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database;
      Stmt : Statement;
      Result : Result_Code;
      Count : Integer;
      Binding_Error : Boolean;
   begin
      Setup_Test_DB (DB);
      
      --  Prepare a parameterized statement
      Prepare (Stmt, DB, "SELECT * FROM test WHERE int_val > ?");
      
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
      Assert (Count = 2, "Should have 2 rows with int_val > 0");
      
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
      Assert (Count = 1, "Should have 1 row with int_val > 100");
      
      --  Reset and clear bindings
      Reset (Stmt);
      Clear_Bindings (Stmt);
      
      --  Try to execute without binding
      Binding_Error := False;
      begin
         Step (Stmt);
      exception
         when SQLite_Error =>
            Binding_Error := True;
      end;
      
      --  Should raise an exception
      Assert (Binding_Error, "Exception should be raised when parameter not bound");
      
      --  Clean up
      Finalize_Statement (Stmt);
      Close (DB);
   end Test_Reset_Clear_Bindings;

   --  Test stepping through results
   procedure Test_Step (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database;
      Stmt : Statement;
      Result : Result_Code;
      Count : Integer;
   begin
      Setup_Test_DB (DB);
      
      --  Prepare a statement
      Prepare (Stmt, DB, "SELECT * FROM test ORDER BY id");
      
      --  Step through results
      Count := 0;
      loop
         Result := Step (Stmt);
         exit when Result = DONE;
         
         if Result = ROW then
            Count := Count + 1;
            
            --  Check row values
            if Count = 1 then
               Assert (Column_Int (Stmt, 1) = 42, "First row int_val should be 42");
               Assert (abs (Column_Double (Stmt, 2) - 3.14) < 0.001, "First row real_val should be 3.14");
               Assert (Column_Text (Stmt, 3) = "hello", "First row text_val should be 'hello'");
            elsif Count = 2 then
               Assert (Column_Int (Stmt, 1) = 123, "Second row int_val should be 123");
               Assert (abs (Column_Double (Stmt, 2) - 2.71) < 0.001, "Second row real_val should be 2.71");
               Assert (Column_Text (Stmt, 3) = "world", "Second row text_val should be 'world'");
            end if;
         end if;
      end loop;
      
      --  Should have 2 rows
      Assert (Count = 2, "Should have 2 rows");
      
      --  Clean up
      Finalize_Statement (Stmt);
      Close (DB);
   end Test_Step;

   --  Test binding NULL values
   procedure Test_Bind_Null (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database;
      Stmt1, Stmt2 : Statement;
      Result : Result_Code;
   begin
      Setup_Test_DB (DB);
      
      --  Prepare an insert statement
      Prepare (Stmt1, DB, "INSERT INTO test (int_val, real_val, text_val, null_val) VALUES (?, ?, ?, ?)");
      
      --  Bind parameters with NULL
      Bind_Int (Stmt1, 1, 999);
      Bind_Double (Stmt1, 2, 9.99);
      Bind_Null (Stmt1, 3);
      Bind_Null (Stmt1, 4);
      
      --  Execute
      Step (Stmt1);
      Finalize_Statement (Stmt1);
      
      --  Verify the inserted row
      Prepare (Stmt2, DB, "SELECT * FROM test WHERE int_val = 999");
      Result := Step (Stmt2);
      
      --  Should have a row
      Assert (Result = ROW, "Should have a row with int_val = 999");
      
      --  Check NULL values
      Assert (Column_Is_Null (Stmt2, 3), "text_val should be NULL");
      Assert (Column_Is_Null (Stmt2, 4), "null_val should be NULL");
      
      --  Clean up
      Finalize_Statement (Stmt2);
      Close (DB);
   end Test_Bind_Null;

   --  Test binding integer values
   procedure Test_Bind_Int (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database;
      Stmt1, Stmt2 : Statement;
      Result : Result_Code;
   begin
      Setup_Test_DB (DB);
      
      --  Prepare an insert statement
      Prepare (Stmt1, DB, "INSERT INTO test (int_val) VALUES (?)");
      
      --  Bind integer parameter
      Bind_Int (Stmt1, 1, 12345);
      
      --  Execute
      Step (Stmt1);
      Finalize_Statement (Stmt1);
      
      --  Verify the inserted row
      Prepare (Stmt2, DB, "SELECT int_val FROM test WHERE int_val = 12345");
      Result := Step (Stmt2);
      
      --  Should have a row
      Assert (Result = ROW, "Should have a row with int_val = 12345");
      Assert (Column_Int (Stmt2, 0) = 12345, "int_val should be 12345");
      
      --  Clean up
      Finalize_Statement (Stmt2);
      Close (DB);
   end Test_Bind_Int;

   --  Test binding 64-bit integer values
   procedure Test_Bind_Int64 (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database;
      Stmt1, Stmt2 : Statement;
      Result : Result_Code;
      Big_Value : constant Long_Integer := 9223372036854775807;
   begin
      Setup_Test_DB (DB);
      
      --  Prepare an insert statement
      Prepare (Stmt1, DB, "INSERT INTO test (int_val) VALUES (?)");
      
      --  Bind 64-bit integer parameter
      Bind_Int64 (Stmt1, 1, Big_Value);
      
      --  Execute
      Step (Stmt1);
      Finalize_Statement (Stmt1);
      
      --  Verify the inserted row
      Prepare (Stmt2, DB, "SELECT int_val FROM test WHERE int_val = ?");
      Bind_Int64 (Stmt2, 1, Big_Value);
      Result := Step (Stmt2);
      
      --  Should have a row
      Assert (Result = ROW, "Should have a row with int_val = Big_Value");
      Assert (Column_Int64 (Stmt2, 0) = Big_Value, "int_val should be Big_Value");
      
      --  Clean up
      Finalize_Statement (Stmt2);
      Close (DB);
   end Test_Bind_Int64;

   --  Test binding floating-point values
   procedure Test_Bind_Double (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database;
      Stmt1, Stmt2 : Statement;
      Result : Result_Code;
      Pi : constant Float := 3.14159265359;
   begin
      Setup_Test_DB (DB);
      
      --  Prepare an insert statement
      Prepare (Stmt1, DB, "INSERT INTO test (real_val) VALUES (?)");
      
      --  Bind double parameter
      Bind_Double (Stmt1, 1, Pi);
      
      --  Execute
      Step (Stmt1);
      Finalize_Statement (Stmt1);
      
      --  Verify the inserted row
      Prepare (Stmt2, DB, "SELECT real_val FROM test WHERE abs(real_val - ?) < 0.0001");
      Bind_Double (Stmt2, 1, Pi);
      Result := Step (Stmt2);
      
      --  Should have a row
      Assert (Result = ROW, "Should have a row with real_val = Pi");
      Assert (abs (Column_Double (Stmt2, 0) - Pi) < 0.0001, "real_val should be Pi");
      
      --  Clean up
      Finalize_Statement (Stmt2);
      Close (DB);
   end Test_Bind_Double;

   --  Test binding text values
   procedure Test_Bind_Text (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database;
      Stmt1, Stmt2 : Statement;
      Result : Result_Code;
      Text_Value : constant String := "This is a test string with special chars: !@#$%^&*()";
   begin
      Setup_Test_DB (DB);
      
      --  Insert a row with text value using Execute
      Execute (DB, "INSERT INTO test (text_val) VALUES ('" & Text_Value & "')");
      
      --  Verify the inserted row using a statement
      Prepare (Stmt2, DB, "SELECT text_val FROM test WHERE text_val = ?");
      Bind_Text (Stmt2, 1, Text_Value);
      Result := Step (Stmt2);
      
      --  Should have a row
      Assert (Result = ROW, "Should have a row with text_val = Text_Value");
      Assert (Column_Text (Stmt2, 0) = Text_Value, "text_val should be Text_Value");
      
      --  Clean up
      Finalize_Statement (Stmt2);
      Close (DB);
   end Test_Bind_Text;

   --  Test binding parameters by name
   procedure Test_Bind_Parameter_Index (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database;
      Stmt : Statement;
      Result : Result_Code;
      Param_Index : Positive;
   begin
      Setup_Test_DB (DB);
      
      --  Prepare a statement with named parameters
      Prepare (Stmt, DB, "SELECT * FROM test WHERE int_val = :value");
      
      --  Get parameter index
      Bind_Parameter_Index (Stmt, ":value", Param_Index);
      
      --  Bind parameter
      Bind_Int (Stmt, Param_Index, 42);
      
      --  Execute
      Result := Step (Stmt);
      
      --  Should have a row
      Assert (Result = ROW, "Should have a row with int_val = 42");
      Assert (Column_Int (Stmt, 1) = 42, "int_val should be 42");
      
      --  Clean up
      Finalize_Statement (Stmt);
      Close (DB);
   end Test_Bind_Parameter_Index;

   --  Test getting column count
   procedure Test_Column_Count (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database;
      Stmt : Statement;
   begin
      Setup_Test_DB (DB);
      
      --  Prepare a statement
      Prepare (Stmt, DB, "SELECT * FROM test");
      
      --  Check column count
      Assert (Column_Count (Stmt) = 5, "Should have 5 columns");
      
      --  Prepare a different statement
      Finalize_Statement (Stmt);
      Prepare (Stmt, DB, "SELECT int_val, text_val FROM test");
      
      --  Check column count
      Assert (Column_Count (Stmt) = 2, "Should have 2 columns");
      
      --  Clean up
      Finalize_Statement (Stmt);
      Close (DB);
   end Test_Column_Count;

   --  Test getting column names
   procedure Test_Column_Name (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database;
      Stmt : Statement;
   begin
      Setup_Test_DB (DB);
      
      --  Prepare a statement
      Prepare (Stmt, DB, "SELECT id, int_val, real_val, text_val, null_val FROM test");
      
      --  Check column names
      Assert (Column_Name (Stmt, 0) = "id", "Column 0 should be 'id'");
      Assert (Column_Name (Stmt, 1) = "int_val", "Column 1 should be 'int_val'");
      Assert (Column_Name (Stmt, 2) = "real_val", "Column 2 should be 'real_val'");
      Assert (Column_Name (Stmt, 3) = "text_val", "Column 3 should be 'text_val'");
      Assert (Column_Name (Stmt, 4) = "null_val", "Column 4 should be 'null_val'");
      
      --  Clean up
      Finalize_Statement (Stmt);
      Close (DB);
   end Test_Column_Name;

   --  Test getting column types
   procedure Test_Column_Type (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database;
      Stmt : Statement;
      Result : Result_Code;
   begin
      Setup_Test_DB (DB);
      
      --  Prepare a statement
      Prepare (Stmt, DB, "SELECT id, int_val, real_val, text_val, null_val FROM test LIMIT 1");
      
      --  Execute
      Result := Step (Stmt);
      Assert (Result = ROW, "Should have a row");
      
      --  Check column types
      Assert (Get_Column_Type (Stmt, 0) = Integer_Type, "Column 0 should be Integer_Type");
      Assert (Get_Column_Type (Stmt, 1) = Integer_Type, "Column 1 should be Integer_Type");
      Assert (Get_Column_Type (Stmt, 2) = Float_Type, "Column 2 should be Float_Type");
      Assert (Get_Column_Type (Stmt, 3) = Text_Type, "Column 3 should be Text_Type");
      Assert (Get_Column_Type (Stmt, 4) = Null_Type, "Column 4 should be Null_Type");
      
      --  Clean up
      Finalize_Statement (Stmt);
      Close (DB);
   end Test_Column_Type;

   --  Test getting integer column values
   procedure Test_Column_Int (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database;
      Stmt : Statement;
      Result : Result_Code;
   begin
      Setup_Test_DB (DB);
      
      --  Prepare a statement
      Prepare (Stmt, DB, "SELECT int_val FROM test WHERE int_val = 42");
      
      --  Execute
      Result := Step (Stmt);
      Assert (Result = ROW, "Should have a row");
      
      --  Check column value
      Assert (Column_Int (Stmt, 0) = 42, "int_val should be 42");
      
      --  Clean up
      Finalize_Statement (Stmt);
      Close (DB);
   end Test_Column_Int;

   --  Test getting 64-bit integer column values
   procedure Test_Column_Int64 (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database;
      Stmt : Statement;
      Result : Result_Code;
      Big_Value : constant Long_Integer := 9223372036854775807;
   begin
      Setup_Test_DB (DB);
      
      --  Insert a row with a big integer
      Execute (DB, "INSERT INTO test (int_val) VALUES (" & Big_Value'Image & ")");
      
      --  Prepare a statement
      Prepare (Stmt, DB, "SELECT int_val FROM test WHERE int_val = " & Big_Value'Image);
      
      --  Execute
      Result := Step (Stmt);
      Assert (Result = ROW, "Should have a row");
      
      --  Check column value
      Assert (Column_Int64 (Stmt, 0) = Big_Value, "int_val should be Big_Value");
      
      --  Clean up
      Finalize_Statement (Stmt);
      Close (DB);
   end Test_Column_Int64;

   --  Test getting floating-point column values
   procedure Test_Column_Double (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database;
      Stmt : Statement;
      Result : Result_Code;
   begin
      Setup_Test_DB (DB);
      
      --  Prepare a statement
      Prepare (Stmt, DB, "SELECT real_val FROM test WHERE abs(real_val - 3.14) < 0.01");
      
      --  Execute
      Result := Step (Stmt);
      Assert (Result = ROW, "Should have a row");
      
      --  Check column value
      Assert (abs (Column_Double (Stmt, 0) - 3.14) < 0.01, "real_val should be approximately 3.14");
      
      --  Clean up
      Finalize_Statement (Stmt);
      Close (DB);
   end Test_Column_Double;

   --  Test getting text column values
   procedure Test_Column_Text (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database;
      Stmt : Statement;
      Result : Result_Code;
   begin
      Setup_Test_DB (DB);
      
      --  Prepare a statement
      Prepare (Stmt, DB, "SELECT text_val FROM test WHERE text_val = 'hello'");
      
      --  Execute
      Result := Step (Stmt);
      Assert (Result = ROW, "Should have a row");
      
      --  Check column value
      Assert (Column_Text (Stmt, 0) = "hello", "text_val should be 'hello'");
      
      --  Clean up
      Finalize_Statement (Stmt);
      Close (DB);
   end Test_Column_Text;

   --  Test checking for NULL column values
   procedure Test_Column_Is_Null (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database;
      Stmt : Statement;
      Result : Result_Code;
   begin
      Setup_Test_DB (DB);
      
      --  Prepare a statement
      Prepare (Stmt, DB, "SELECT null_val FROM test LIMIT 1");
      
      --  Execute
      Result := Step (Stmt);
      Assert (Result = ROW, "Should have a row");
      
      --  Check column value
      Assert (Column_Is_Null (Stmt, 0), "null_val should be NULL");
      
      --  Clean up
      Finalize_Statement (Stmt);
      Close (DB);
   end Test_Column_Is_Null;

   --  Test invalid statement operations
   procedure Test_Invalid_Statement (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database;
      Stmt : Statement;
      Exception_Raised : Boolean;
   begin
      Setup_Test_DB (DB);
      
      --  Try to use an uninitialized statement
      Exception_Raised := False;
      begin
         Step (Stmt);
      exception
         when SQLite_Error =>
            Exception_Raised := True;
      end;
      Assert (Exception_Raised, "Exception should be raised for uninitialized statement");
      
      --  Try to prepare an invalid SQL statement
      Exception_Raised := False;
      begin
         Prepare (Stmt, DB, "SELECT * FROM nonexistent_table");
         Step (Stmt);
      exception
         when SQLite_Error =>
            Exception_Raised := True;
      end;
      Assert (Exception_Raised, "Exception should be raised for invalid SQL");
      
      --  Clean up
      Close (DB);
   end Test_Invalid_Statement;

   --  Register test routines to call
   package Caller is new AUnit.Test_Caller (Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite := 
         new AUnit.Test_Suites.Test_Suite;
   begin
      Result.Add_Test (Caller.Create ("Test_Prepare_Finalize", Test_Prepare_Finalize'Access));
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
