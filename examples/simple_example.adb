-------------------------------------------------------------------------------
-- Simple example demonstrating the use of Ada_Sqlite3
--
-- Copyright (c) 2025 Garret Noble
--
-- Licensed under the terms of the MIT License or Apache-2.0 with LLVM exception
-------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Exceptions;
with Ada_Sqlite3;

procedure Simple_Example is
   use Ada.Text_IO;
   use Ada_Sqlite3;

   Stmt : Statement;
   Result : Result_Code;
begin
   Put_Line ("SQLite3 version: " & Version);
   Put_Line ("Opening database...");

   --  Open an in-memory database
   declare
      DB : Database := Open (":memory:", OPEN_READWRITE or OPEN_CREATE);
   begin
      Put_Line ("Database opened successfully");

      --  Create a table
      Put_Line ("Creating table...");
      Execute (DB, "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)");

      --  Insert data using Execute
      Put_Line ("Inserting data...");
      Execute (DB, "INSERT INTO users (name, age) VALUES ('Alice', 30)");
      Execute (DB, "INSERT INTO users (name, age) VALUES ('Bob', 25)");
      Execute (DB, "INSERT INTO users (name, age) VALUES ('Charlie', 35)");

      --  Insert data using prepared statement
      Put_Line ("Inserting data with prepared statement...");
      Stmt := Prepare (DB, "INSERT INTO users (name, age) VALUES (?, ?)");

      --  Insert first record
      Bind_Text (Stmt, 1, "David");
      Bind_Int (Stmt, 2, 40);
      Step (Stmt);
      Reset (Stmt);
      Clear_Bindings (Stmt);

      --  Insert second record
      Bind_Text (Stmt, 1, "Eve");
      Bind_Int (Stmt, 2, 22);
      Step (Stmt);

      --  Query data
      Put_Line ("Querying data...");
      Stmt := Prepare (DB, "SELECT id, name, age FROM users ORDER BY age");

      --  Print column names
      Put_Line ("Results:");
      Put_Line ("ID | Name    | Age");
      Put_Line ("---+---------+----");

      --  Fetch rows
      loop
         Result := Step (Stmt);
         exit when Result = DONE;

         if Result = ROW then
            --  Print row data
            Put (Column_Int (Stmt, 0)'Image & " | ");
            Put (Column_Text (Stmt, 1) & " | ");
            Put_Line (Column_Int (Stmt, 2)'Image);
         end if;
      end loop;

      --  Clean up
      
      Put_Line ("Database closed");
   end;

exception
   when E : SQLite_Error =>
      Put_Line ("SQLite error: " & Ada.Exceptions.Exception_Message (E));
   when E : others =>
      Put_Line ("Error: " & Ada.Exceptions.Exception_Message (E));
end Simple_Example;
