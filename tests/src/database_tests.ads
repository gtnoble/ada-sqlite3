-------------------------------------------------------------------------------
-- Database Tests for Ada_Sqlite3
--
-- Copyright (c) 2025 Garret Noble
--
-- Licensed under the terms of the MIT License or Apache-2.0 with LLVM exception
-------------------------------------------------------------------------------

with AUnit;
with AUnit.Test_Fixtures;
with AUnit.Test_Suites;

package Database_Tests is

   --  Test case for Database operations
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   --  Test routines
   procedure Test_Open_Close (T : in out Test);
   procedure Test_In_Memory_Database (T : in out Test);
   procedure Test_Execute (T : in out Test);
   procedure Test_Last_Insert_Row_ID (T : in out Test);
   procedure Test_Changes (T : in out Test);
   procedure Test_Version (T : in out Test);
   procedure Test_Invalid_Database (T : in out Test);
   procedure Test_Invalid_SQL (T : in out Test);

   --  Register test routines to call
   function Suite return AUnit.Test_Suites.Access_Test_Suite;

end Database_Tests;
