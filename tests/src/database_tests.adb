-------------------------------------------------------------------------------
-- Database Tests for Ada_Sqlite3
--
-- Copyright (c) 2025 Garret Noble
--
-- Licensed under the terms of the MIT License or Apache-2.0 with LLVM exception
-------------------------------------------------------------------------------

with Ada.Directories;
with Ada.Exceptions;
with AUnit.Assertions;
with AUnit.Test_Caller;
with Ada_Sqlite3;

package body Database_Tests is

   use AUnit.Assertions;
   use Ada_Sqlite3;

   --  Test temporary database file
   Test_DB_File : constant String := "test_database.db";

   --  Clean up any existing test database file
   procedure Remove_Test_DB is
   begin
      if Ada.Directories.Exists (Test_DB_File) then
         Ada.Directories.Delete_File (Test_DB_File);
      end if;
   end Remove_Test_DB;

   --  Test opening and closing a database
   procedure Test_Open_Close (T : in out Test) is
      pragma Unreferenced (T);
   begin
      --  Clean up any existing test file
      Remove_Test_DB;

      --  Test opening a file database
      declare
         DB : Database := Open (Test_DB_File, OPEN_READWRITE or OPEN_CREATE);
      begin
         Assert (Is_Open (DB), "Expected database to be open but it was closed");

         --  Create and use a statement
         declare
            Stmt : Statement := Prepare (DB, "CREATE TABLE test (id INTEGER PRIMARY KEY)");
         begin
            Step (Stmt);
            --  Statement will be finalized here when it goes out of scope
         end;

         --  Database will be finalized here when it goes out of scope
      end;

      --  Clean up
      Remove_Test_DB;
   end Test_Open_Close;

   --  Test in-memory database
   procedure Test_In_Memory_Database (T : in out Test) is
      pragma Unreferenced (T);
   begin
      --  Open an in-memory database
      declare
         DB : Database := Open (":memory:", OPEN_READWRITE or OPEN_CREATE);
      begin
         Assert (Is_Open (DB), "Expected in-memory database to be open but it was closed");

         --  Create a table and insert data
         Execute (DB, "CREATE TABLE test (id INTEGER PRIMARY KEY, value TEXT)");
         Execute (DB, "INSERT INTO test (value) VALUES ('test value')");

         --  Verify data was inserted
         declare
            Stmt : Statement := Prepare (DB, "SELECT COUNT(*) FROM test");
            Result : constant Result_Code := Step (Stmt);
         begin
            Assert (Result = ROW, "Expected ROW but got " & Result_Code'Image(Result));
            Assert (Column_Int (Stmt, 0) = 1, 
                    "Expected 1 row but got" & Integer'Image(Column_Int (Stmt, 0)));
         end;

      end;
   end Test_In_Memory_Database;

   --  Test executing SQL statements
   procedure Test_Execute (T : in out Test) is
      pragma Unreferenced (T);
   begin
      --  Open an in-memory database
      declare
         DB : Database := Open (":memory:", OPEN_READWRITE or OPEN_CREATE);
      begin
         --  Execute multiple statements
         Execute (DB, "CREATE TABLE test (id INTEGER PRIMARY KEY, value TEXT)");
         Execute (DB, "INSERT INTO test (value) VALUES ('value1')");
         Execute (DB, "INSERT INTO test (value) VALUES ('value2')");

         --  Verify data was inserted
         declare
            Stmt : Statement := Prepare (DB, "SELECT * FROM test");
            Result : Result_Code;
            Count : Integer := 0;
         begin
            
            loop
               Result := Step (Stmt);
               exit when Result = DONE;
               
               if Result = ROW then
                  Count := Count + 1;
               end if;
            end loop;
            
            Assert (Count = 2, "Expected 2 rows but got" & Integer'Image(Count));
         end;

      end;
   end Test_Execute;

   --  Test last insert row ID
   procedure Test_Last_Insert_Row_ID (T : in out Test) is
      pragma Unreferenced (T);
   begin
      --  Open an in-memory database
      declare
         DB : Database := Open (":memory:", OPEN_READWRITE or OPEN_CREATE);
      begin
         --  Create a table and insert data
         Execute (DB, "CREATE TABLE test (id INTEGER PRIMARY KEY, value TEXT)");
         Execute (DB, "INSERT INTO test (value) VALUES ('test value')");

         --  Check last insert row ID
         Assert (Last_Insert_Row_ID (DB) = 1, 
                 "Expected last insert row ID to be 1 but got" & Long_Integer'Image(Last_Insert_Row_ID (DB)));

         --  Insert another row
         Execute (DB, "INSERT INTO test (value) VALUES ('another value')");

         --  Check last insert row ID
         Assert (Last_Insert_Row_ID (DB) = 2, 
                 "Expected last insert row ID to be 2 but got" & Long_Integer'Image(Last_Insert_Row_ID (DB)));
      end;
   end Test_Last_Insert_Row_ID;

   --  Test changes count
   procedure Test_Changes (T : in out Test) is
      pragma Unreferenced (T);
   begin
      --  Open an in-memory database
      declare
         DB : Database := Open (":memory:", OPEN_READWRITE or OPEN_CREATE);
      begin
         --  Create a table and insert data
         Execute (DB, "CREATE TABLE test (id INTEGER PRIMARY KEY, value TEXT)");
         Execute (DB, "INSERT INTO test (value) VALUES ('value1')");
         Execute (DB, "INSERT INTO test (value) VALUES ('value2')");
         Execute (DB, "INSERT INTO test (value) VALUES ('value3')");

         --  Update rows
         Execute (DB, "UPDATE test SET value = 'updated'");

         --  Check number of changes
         Assert (Changes (DB) = 3, "Expected 3 changes but got" & Integer'Image(Changes (DB)));
      end;
   end Test_Changes;

   --  Test version string
   procedure Test_Version (T : in out Test) is
      pragma Unreferenced (T);
      Ver : constant String := Version;
   begin
      --  Version should not be empty
      Assert (Ver'Length > 0, 
              "Expected non-empty version string but got length" & Integer'Image(Ver'Length));
   end Test_Version;

   --  Test invalid database
   procedure Test_Invalid_Database (T : in out Test) is
      pragma Unreferenced (T);
      Exception_Raised : Boolean := False;
   begin
      --  Try to open a database in a non-existent directory
      begin
         declare
            DB : Database := Open ("/non/existent/path/db.sqlite", OPEN_READWRITE);
            pragma Unreferenced (DB);
         begin
            null;
         end;
      exception
         when SQLite_Error =>
            Exception_Raised := True;
      end;

      --  Check that an exception was raised
      Assert (Exception_Raised, 
              "Expected SQLite_Error exception for invalid database path but no exception was raised");
   end Test_Invalid_Database;

   --  Test invalid SQL
   procedure Test_Invalid_SQL (T : in out Test) is
      pragma Unreferenced (T);
      Exception_Raised : Boolean := False;
   begin
      --  Open an in-memory database
      declare
         DB : Database := Open (":memory:", OPEN_READWRITE or OPEN_CREATE);
      begin
         --  Try to execute invalid SQL
         begin
            Execute (DB, "CREATE INVALID SYNTAX");
         exception
            when SQLite_Error =>
               Exception_Raised := True;
         end;

         --  Check that an exception was raised
         Assert (Exception_Raised, 
                 "Expected SQLite_Error exception for invalid SQL but no exception was raised");
      end;
   end Test_Invalid_SQL;

   --  Register test routines to call
   package Caller is new AUnit.Test_Caller (Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite := 
         new AUnit.Test_Suites.Test_Suite;
   begin
      Result.Add_Test (Caller.Create ("Test_Open_Close", Test_Open_Close'Access));
      Result.Add_Test (Caller.Create ("Test_In_Memory_Database", Test_In_Memory_Database'Access));
      Result.Add_Test (Caller.Create ("Test_Execute", Test_Execute'Access));
      Result.Add_Test (Caller.Create ("Test_Last_Insert_Row_ID", Test_Last_Insert_Row_ID'Access));
      Result.Add_Test (Caller.Create ("Test_Changes", Test_Changes'Access));
      Result.Add_Test (Caller.Create ("Test_Version", Test_Version'Access));
      Result.Add_Test (Caller.Create ("Test_Invalid_Database", Test_Invalid_Database'Access));
      Result.Add_Test (Caller.Create ("Test_Invalid_SQL", Test_Invalid_SQL'Access));
      
      return Result;
   end Suite;

end Database_Tests;
