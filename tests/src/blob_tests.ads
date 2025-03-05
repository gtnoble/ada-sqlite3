-------------------------------------------------------------------------------
-- Blob Tests for Ada_Sqlite3
--
-- Copyright (c) 2025 Garret Noble
--
-- Licensed under the terms of the MIT License or Apache-2.0 with LLVM exception
-------------------------------------------------------------------------------

with AUnit;
with AUnit.Test_Fixtures;
with AUnit.Test_Suites;

package Blob_Tests is

   --  Test case for Blob operations
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   --  Test routines
   procedure Test_Create_Blob (T : in out Test);
   procedure Test_Blob_Size (T : in out Test);
   procedure Test_Blob_Data (T : in out Test);
   procedure Test_Bind_Blob (T : in out Test);
   procedure Test_Column_Blob (T : in out Test);
   procedure Test_Read_Write_Blob (T : in out Test);
   procedure Test_Empty_Blob (T : in out Test);
   procedure Test_Large_Blob (T : in out Test);

   --  Register test routines to call
   function Suite return AUnit.Test_Suites.Access_Test_Suite;

end Blob_Tests;
