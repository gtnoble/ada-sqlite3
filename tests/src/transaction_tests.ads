-------------------------------------------------------------------------------
-- Transaction Tests for Ada_Sqlite3
--
-- Copyright (c) 2025 Garret Noble
--
-- Licensed under the terms of the MIT License or Apache-2.0 with LLVM exception
-------------------------------------------------------------------------------

with AUnit;
with AUnit.Test_Fixtures;
with AUnit.Test_Suites;

package Transaction_Tests is

   --  Test case for Transaction operations
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   --  Test routines
   procedure Test_Begin_Commit (T : in out Test);
   procedure Test_Begin_Rollback (T : in out Test);
   procedure Test_Nested_Transactions (T : in out Test);
   procedure Test_Transaction_Isolation (T : in out Test);
   procedure Test_Transaction_Error (T : in out Test);

   --  Register test routines to call
   function Suite return AUnit.Test_Suites.Access_Test_Suite;

end Transaction_Tests;
