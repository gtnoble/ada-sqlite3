-------------------------------------------------------------------------------
-- Transaction example demonstrating the use of Ada_Sqlite3 with transactions
--
-- Copyright (c) 2025 Garret Noble
--
-- Licensed under the terms of the MIT License or Apache-2.0 with LLVM exception
-------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Exceptions;
with Ada_Sqlite3;

procedure Transaction_Example is
   use Ada.Text_IO;
   use Ada_Sqlite3;

   Stmt : Statement;
   Result : Result_Code;

   --  Helper procedure to print all users
   procedure Print_Users (DB : in out Database) is
      Query_Stmt : Statement;
   begin
      Put_Line ("Current users:");
      Put_Line ("ID | Name    | Age");
      Put_Line ("---+---------+----");

      Query_Stmt := Prepare (DB, "SELECT id, name, age FROM users ORDER BY id");

      loop
         Result := Step (Query_Stmt);
         exit when Result = DONE;

         if Result = ROW then
            Put (Column_Int (Query_Stmt, 0)'Image & " | ");
            Put (Column_Text (Query_Stmt, 1) & " | ");
            Put_Line (Column_Int (Query_Stmt, 2)'Image);
         end if;
      end loop;

      New_Line;
   end Print_Users;

begin
   Put_Line ("SQLite3 Transaction Example");
   Put_Line ("Opening database...");

   --  Open an in-memory database
   declare
      DB : Database := Open (":memory:", OPEN_READWRITE or OPEN_CREATE);
   begin
      Put_Line ("Database opened successfully");

      --  Create a table
      Put_Line ("Creating table...");
      Execute (DB, "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)");

      --  Insert initial data
      Put_Line ("Inserting initial data...");
      Execute (DB, "INSERT INTO users (name, age) VALUES ('Alice', 30)");
      Execute (DB, "INSERT INTO users (name, age) VALUES ('Bob', 25)");

      --  Print initial data
      Print_Users (DB);

      --  Begin a transaction
      Put_Line ("Beginning transaction...");
      Execute (DB, "BEGIN TRANSACTION");

      --  Insert data within the transaction
      Put_Line ("Inserting data within transaction...");
      Execute (DB, "INSERT INTO users (name, age) VALUES ('Charlie', 35)");
      Execute (DB, "INSERT INTO users (name, age) VALUES ('David', 40)");

      --  Print data during transaction
      Put_Line ("Data during transaction:");
      Print_Users (DB);

      --  Rollback the transaction
      Put_Line ("Rolling back transaction...");
      Execute (DB, "ROLLBACK");

      --  Print data after rollback
      Put_Line ("Data after rollback:");
      Print_Users (DB);

      --  Begin another transaction
      Put_Line ("Beginning another transaction...");
      Execute (DB, "BEGIN TRANSACTION");

      --  Insert data within the transaction
      Put_Line ("Inserting data within transaction...");
      Execute (DB, "INSERT INTO users (name, age) VALUES ('Eve', 22)");
      Execute (DB, "INSERT INTO users (name, age) VALUES ('Frank', 45)");

      --  Print data during transaction
      Put_Line ("Data during transaction:");
      Print_Users (DB);

      --  Commit the transaction
      Put_Line ("Committing transaction...");
      Execute (DB, "COMMIT");

      --  Print data after commit
      Put_Line ("Data after commit:");
      Print_Users (DB);

      --  Clean up
      Put_Line ("Database closed");
   end;

exception
   when E : SQLite_Error =>
      Put_Line ("SQLite error: " & Ada.Exceptions.Exception_Message (E));
   when E : others =>
      Put_Line ("Error: " & Ada.Exceptions.Exception_Message (E));
end Transaction_Example;
