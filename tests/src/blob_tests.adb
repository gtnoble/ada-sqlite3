-------------------------------------------------------------------------------
-- Blob Tests for Ada_Sqlite3
--
-- Copyright (c) 2025 Garret Noble
--
-- Licensed under the terms of the MIT License or Apache-2.0 with LLVM exception
-------------------------------------------------------------------------------

with AUnit.Assertions;
with AUnit.Test_Caller;
with Ada.Streams;
with Ada_Sqlite3;
with Ada_Sqlite3.Blobs;

package body Blob_Tests is

   use AUnit.Assertions;
   use Ada.Streams;
   use Ada_Sqlite3;
   use Ada_Sqlite3.Blobs;

   --  Setup test database
   function Setup_Test_DB return Database is
   begin
      --  Open an in-memory database
      return DB : Database := Open (":memory:", OPEN_READWRITE or OPEN_CREATE) do
         --  Create a test table with a BLOB column
         Execute (DB, "CREATE TABLE test_blobs (id INTEGER PRIMARY KEY, name TEXT, data BLOB)");
      end return;
   end Setup_Test_DB;

   --  Helper function to compare stream element arrays
   function Arrays_Equal (
      Left  : Stream_Element_Array;
      Right : Stream_Element_Array) return Boolean is
   begin
      if Left'Length /= Right'Length then
         return False;
      end if;
      
      for I in Left'Range loop
         if Left (I) /= Right (I - Left'First + Right'First) then
            return False;
         end if;
      end loop;
      
      return True;
   end Arrays_Equal;

   --  Test creating a blob
   procedure Test_Create_Blob (T : in out Test) is
      pragma Unreferenced (T);
      Test_Data : constant Stream_Element_Array := (1, 2, 3, 4, 5);
      Test_Blob : Blob;
   begin
      --  Create a blob from data
      Test_Blob := Create (Test_Data);
      
      --  Check that the blob was created correctly
      Assert (Size (Test_Blob) = Natural (Test_Data'Length), "Blob size should match data size");
      Assert (Arrays_Equal (Data (Test_Blob), Test_Data), "Blob data should match original data");
   end Test_Create_Blob;

   --  Test blob size
   procedure Test_Blob_Size (T : in out Test) is
      pragma Unreferenced (T);
      Empty_Data : constant Stream_Element_Array := (1 .. 0 => 0);
      Small_Data : constant Stream_Element_Array := (1, 2, 3);
      Large_Data : constant Stream_Element_Array := (1 .. 1000 => 42);
      
      Empty_Blob : constant Blob := Create (Empty_Data);
      Small_Blob : constant Blob := Create (Small_Data);
      Large_Blob : constant Blob := Create (Large_Data);
   begin
      --  Check sizes
      Assert (Size (Empty_Blob) = 0, "Empty blob size should be 0");
      Assert (Size (Small_Blob) = 3, "Small blob size should be 3");
      Assert (Size (Large_Blob) = 1000, "Large blob size should be 1000");
   end Test_Blob_Size;

   --  Test blob data
   procedure Test_Blob_Data (T : in out Test) is
      pragma Unreferenced (T);
      Test_Data : constant Stream_Element_Array := (10, 20, 30, 40, 50);
      Test_Blob : constant Blob := Create (Test_Data);
      Retrieved_Data : constant Stream_Element_Array := Data (Test_Blob);
   begin
      --  Check that the retrieved data matches the original
      Assert (Stream_Element_Offset (Retrieved_Data'Length) = Stream_Element_Offset (Test_Data'Length), "Retrieved data length should match original");
      
      for I in Test_Data'Range loop
         Assert (Retrieved_Data (I - Test_Data'First + Retrieved_Data'First) = Test_Data (I),
                "Data element " & Stream_Element_Offset'Image (I) & " should match");
      end loop;
   end Test_Blob_Data;

   --  Test binding a blob to a statement
   procedure Test_Bind_Blob (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database := Open (":memory:", OPEN_READWRITE or OPEN_CREATE);
      Test_Data : constant Stream_Element_Array := (1, 2, 3, 4, 5, 6, 7, 8);
      Test_Blob : constant Blob := Create (Test_Data);
   begin
      
      --  Create a test table with a BLOB column
      Execute (DB, "CREATE TABLE test_blobs (id INTEGER PRIMARY KEY, name TEXT, data BLOB)");
      
      --  Insert a blob
      declare
         Stmt : Statement := Prepare (DB, "INSERT INTO test_blobs (name, data) VALUES (?, ?)");
      begin
         Bind_Text (Stmt, 1, "Test Blob");
         Bind_Blob (Stmt, 2, Test_Blob);
         Step (Stmt);
      end;
      
      
      --  Verify the blob was inserted
      declare
         Stmt2 : Statement := Prepare (DB, "SELECT data FROM test_blobs WHERE name = 'Test Blob'");
      begin
         Assert (Step (Stmt2) = ROW, "Should have a row with the inserted blob");
      end;
      
      --  Clean up
      
      
   end Test_Bind_Blob;

   --  Test retrieving a blob from a column
   procedure Test_Column_Blob (T : in out Test) is
      pragma Unreferenced (T);
      DB : Database := Open (":memory:", OPEN_READWRITE or OPEN_CREATE);
      Test_Data : constant Stream_Element_Array := (1, 2, 3, 4, 5, 6, 7, 8);
      Test_Blob : constant Blob := Create (Test_Data);
      Retrieved_Blob : Blob;
   begin
      --  Open database
      
      --  Create a test table with a BLOB column
      Execute (DB, "CREATE TABLE test_blobs (id INTEGER PRIMARY KEY, name TEXT, data BLOB)");
      
      --  Insert a blob
      declare
         Stmt : Statement := Prepare (DB, "INSERT INTO test_blobs (name, data) VALUES (?, ?)");
      begin
         Bind_Text (Stmt, 1, "Test Blob");
         Bind_Blob (Stmt, 2, Test_Blob);
         Step (Stmt);
      end;
      
      
      --  Retrieve the blob
      declare
         Stmt2 : Statement := Prepare (DB, "SELECT data FROM test_blobs WHERE name = 'Test Blob'");
      begin
         Assert (Step (Stmt2) = ROW, "Should have a row with the inserted blob");
      
         --  Get the blob from the column
         Retrieved_Blob := Column_Blob (Stmt2, 0);
      
         --  Check that the retrieved blob matches the original
         Assert (Size (Retrieved_Blob) = Size (Test_Blob), "Retrieved blob size should match original");
         Assert (Arrays_Equal (Data (Retrieved_Blob), Data (Test_Blob)), 
                "Retrieved blob data should match original");
      end;
      
      --  Clean up
      
      
   end Test_Column_Blob;

   --  Test reading and writing blobs
   procedure Test_Read_Write_Blob (T : in out Test) is
      pragma Unreferenced (T);
      Initial_Data : constant Stream_Element_Array := (1, 2, 3, 4, 5);
      Test_Blob : Blob := Create (Initial_Data);
      
      --  Buffer for reading
      Buffer : Stream_Element_Array (1 .. 10);
      Last : Stream_Element_Offset;
      
      --  Data to write
      Write_Data : constant Stream_Element_Array := (10, 20, 30);
   begin
      --  Test reading
      Read_Blob (Test_Blob, Buffer, Last);
      
      --  Check that the read was successful
      Assert (Last = Stream_Element_Offset (5), "Should have read 5 elements");
      Assert (Arrays_Equal (Buffer (1 .. 5), Initial_Data), "Read data should match initial data");
      
      --  Test writing
      Write_Blob (Test_Blob, Write_Data);
      
      --  Check that the write was successful
      Assert (Size (Test_Blob) = Natural (Initial_Data'Length + Write_Data'Length), 
             "Blob size should be sum of initial and written data");
      
      --  Read the entire blob
      declare
         Full_Buffer : Stream_Element_Array (1 .. Stream_Element_Offset (Size (Test_Blob)));
         Full_Last : Stream_Element_Offset;
      begin
         --  Create a new blob to read from the beginning
         Test_Blob := Create (Data (Test_Blob));
         
         --  Read the entire blob
         Read_Blob (Test_Blob, Full_Buffer, Full_Last);
         
         --  Check that we read the expected data
         Assert (Full_Last = Stream_Element_Offset (Size (Test_Blob)), "Should have read the entire blob");
         
      --  Check that the first part matches the initial data
      for I in Initial_Data'Range loop
         declare
            Buffer_Index : constant Stream_Element_Offset := I - Initial_Data'First + Stream_Element_Offset (1);
         begin
            Assert (Full_Buffer (Buffer_Index) = Initial_Data (I),
                   "First part should match initial data");
         end;
      end loop;
         
         --  Check that the second part matches the written data
         for I in Write_Data'Range loop
            declare
               Buffer_Index : constant Stream_Element_Offset := 
                  I - Write_Data'First + Stream_Element_Offset (Initial_Data'Length) + Stream_Element_Offset (1);
            begin
               Assert (Full_Buffer (Buffer_Index) = Write_Data (I),
                      "Second part should match written data");
            end;
         end loop;
      end;
   end Test_Read_Write_Blob;

   --  Test empty blob
   procedure Test_Empty_Blob (T : in out Test) is
      pragma Unreferenced (T);
      Empty_Data : constant Stream_Element_Array (1 .. 0) := (others => 0);
      Empty_Blob : constant Blob := Create (Empty_Data);
      DB : Database := Open (":memory:", OPEN_READWRITE or OPEN_CREATE);
      Retrieved_Blob : Blob;
   begin
      
      --  Create a test table with a BLOB column
      Execute (DB, "CREATE TABLE test_blobs (id INTEGER PRIMARY KEY, name TEXT, data BLOB)");
      
      --  Test storing and retrieving empty blob
      declare
         Stmt : Statement := Prepare (DB, "INSERT INTO test_blobs (name, data) VALUES (?, ?)");
      begin
         Bind_Text (Stmt, 1, "Empty Blob");
         Bind_Blob (Stmt, 2, Empty_Blob);
         Step (Stmt);
      end;
      
      
      --  Retrieve the empty blob
      declare
         Stmt2 : Statement := Prepare (DB, "SELECT data FROM test_blobs WHERE name = 'Empty Blob'");
      begin
         Assert (Step (Stmt2) = ROW, "Should have a row with the empty blob");
      
         --  Get the blob from the column
         Retrieved_Blob := Column_Blob (Stmt2, 0);
      
         --  Check that the retrieved blob is empty
         Assert (Size (Retrieved_Blob) = 0, "Retrieved empty blob size should be 0");
      end;
      
      --  Clean up
      
      
   end Test_Empty_Blob;

   --  Test large blob
   procedure Test_Large_Blob (T : in out Test) is
      pragma Unreferenced (T);
      --  Create a 100KB blob
      Large_Size : constant Stream_Element_Offset := 100 * 1024;
      Large_Data : Stream_Element_Array (1 .. Large_Size);
      Large_Blob : Blob;
      DB : Database := Open (":memory:", OPEN_READWRITE or OPEN_CREATE);
      Retrieved_Blob : Blob;
   begin
      
      --  Create a test table with a BLOB column
      Execute (DB, "CREATE TABLE test_blobs (id INTEGER PRIMARY KEY, name TEXT, data BLOB)");
      
      --  Fill the large data array with a pattern
      for I in Large_Data'Range loop
         Large_Data (I) := Stream_Element (I mod 256);
      end loop;
      
      --  Create the large blob
      Large_Blob := Create (Large_Data);
      
      --  Check large blob properties
      Assert (Size (Large_Blob) = Natural (Large_Size), "Large blob size should match");
      Assert (Stream_Element_Offset (Data (Large_Blob)'Length) = Large_Size, "Large blob data length should match");
      
      --  Test storing and retrieving large blob
      declare
         Stmt : Statement := Prepare (DB, "INSERT INTO test_blobs (name, data) VALUES (?, ?)");
      begin
         Bind_Text (Stmt, 1, "Large Blob");
         Bind_Blob (Stmt, 2, Large_Blob);
         Step (Stmt);
      end;
      
      
      --  Retrieve the large blob
      declare
         Stmt2 : Statement := Prepare (DB, "SELECT data FROM test_blobs WHERE name = 'Large Blob'");
      begin
         Assert (Step (Stmt2) = ROW, "Should have a row with the large blob");
      
         --  Get the blob from the column
         Retrieved_Blob := Column_Blob (Stmt2, 0);
      
         --  Check that the retrieved blob matches the original
         Assert (Size (Retrieved_Blob) = Size (Large_Blob), "Retrieved large blob size should match");
      
         --  Check a sample of the data (checking all 100KB would be too slow for a unit test)
         declare
            Original_Data : constant Stream_Element_Array := Data (Large_Blob);
            Retrieved_Data : constant Stream_Element_Array := Data (Retrieved_Blob);
         
            --  Check the first 100 bytes
            First_Range : constant Stream_Element_Offset := 100;
         
            --  Check the last 100 bytes
            Last_Start : constant Stream_Element_Offset := Large_Size - 100 + 1;
         begin
            --  Check first chunk
            for I in 1 .. First_Range loop
               Assert (Retrieved_Data (I) = Original_Data (I),
                      "First chunk data should match at index " & Stream_Element_Offset'Image (I));
            end loop;
         
            --  Check last chunk
            for I in Last_Start .. Large_Size loop
               Assert (Retrieved_Data (I) = Original_Data (I),
                      "Last chunk data should match at index " & Stream_Element_Offset'Image (I));
            end loop;
         
            --  Check some random samples in the middle
            for I in 1 .. 10 loop
               declare
                  Index : constant Stream_Element_Offset := 
                     Stream_Element_Offset (I * Natural (Large_Size) / 10);
               begin
                  Assert (Retrieved_Data (Index) = Original_Data (Index),
                         "Middle sample should match at index " & Stream_Element_Offset'Image (Index));
               end;
            end loop;
         end;
      end;
      
      --  Clean up
      
      
   end Test_Large_Blob;

   --  Register test routines to call
   package Caller is new AUnit.Test_Caller (Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite := 
         new AUnit.Test_Suites.Test_Suite;
   begin
      Result.Add_Test (Caller.Create ("Test_Create_Blob", Test_Create_Blob'Access));
      Result.Add_Test (Caller.Create ("Test_Blob_Size", Test_Blob_Size'Access));
      Result.Add_Test (Caller.Create ("Test_Blob_Data", Test_Blob_Data'Access));
      Result.Add_Test (Caller.Create ("Test_Bind_Blob", Test_Bind_Blob'Access));
      Result.Add_Test (Caller.Create ("Test_Column_Blob", Test_Column_Blob'Access));
      Result.Add_Test (Caller.Create ("Test_Read_Write_Blob", Test_Read_Write_Blob'Access));
      Result.Add_Test (Caller.Create ("Test_Empty_Blob", Test_Empty_Blob'Access));
      Result.Add_Test (Caller.Create ("Test_Large_Blob", Test_Large_Blob'Access));
      
      return Result;
   end Suite;

end Blob_Tests;
