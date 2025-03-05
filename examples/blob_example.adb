-------------------------------------------------------------------------------
-- BLOB example demonstrating the use of Ada_Sqlite3 with binary data
--
-- Copyright (c) 2025 Garret Noble
--
-- Licensed under the terms of the MIT License or Apache-2.0 with LLVM exception
-------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Streams;
with Ada.Exceptions;
with Ada_Sqlite3;
with Ada_Sqlite3.Blobs;

procedure Blob_Example is
   use Ada.Text_IO;
   use Ada.Streams;
   use Ada_Sqlite3;
   use Ada_Sqlite3.Blobs;

   Result : Result_Code;

   --  Create some sample binary data
   Sample_Data : constant Stream_Element_Array :=
     (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16);

   --  Create a BLOB from the sample data
   Sample_Blob : Blob := Create (Sample_Data);

   --  Helper function to print binary data in hexadecimal format
   procedure Print_Hex (Data : Stream_Element_Array) is
   begin
      for I in Data'Range loop
         Put (Stream_Element'Image (Data (I)) & " (0x" &
              Integer'Image (Integer (Data (I)))(2 .. 3) & ") ");
         if (I - Data'First + 1) mod 8 = 0 then
            New_Line;
         end if;
      end loop;
      New_Line;
   end Print_Hex;

begin
   Put_Line ("SQLite3 BLOB Example");
   Put_Line ("Opening database...");

   --  Open an in-memory database
   declare
      DB : Database := Open (":memory:", OPEN_READWRITE or OPEN_CREATE);
   begin
      Put_Line ("Database opened successfully");

      --  Create a table with a BLOB column
      Put_Line ("Creating table...");
      Execute (DB, "CREATE TABLE blobs (id INTEGER PRIMARY KEY, name TEXT, data BLOB)");

      --  Insert data using prepared statement
      Put_Line ("Inserting BLOB data...");
      declare
         Stmt : Statement := Prepare (DB, "INSERT INTO blobs (name, data) VALUES (?, ?)");
      begin
         --  Insert first record
         Bind_Text (Stmt, 1, "Sample BLOB");
         Bind_Blob (Stmt, 2, Sample_Blob);
         Step (Stmt);
      end;

      --  Query data
      Put_Line ("Querying BLOB data...");
      declare
         Stmt : Statement := Prepare (DB, "SELECT id, name, data FROM blobs");
      begin
         --  Fetch rows
         loop
         Result := Step (Stmt);
         exit when Result = DONE;

         if Result = ROW then
            --  Print row data
            Put_Line ("ID: " & Column_Int (Stmt, 0)'Image);
            Put_Line ("Name: " & Column_Text (Stmt, 1));
            
            --  Get and print BLOB data
            declare
               Blob_Data : constant Blob := Column_Blob (Stmt, 2);
               Raw_Data : constant Stream_Element_Array := Data (Blob_Data);
            begin
               Put_Line ("BLOB Size: " & Size (Blob_Data)'Image & " bytes");
               Put_Line ("BLOB Data (hex):");
               Print_Hex (Raw_Data);
            end;
         end if;
      end loop;

         --  Clean up
      end;
      
      Put_Line ("Database closed");
   end;

exception
   when E : SQLite_Error =>
      Put_Line ("SQLite error: " & Ada.Exceptions.Exception_Message (E));
   when E : others =>
      Put_Line ("Error: " & Ada.Exceptions.Exception_Message (E));
end Blob_Example;
