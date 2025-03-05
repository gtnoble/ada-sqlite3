-------------------------------------------------------------------------------
-- Statement Tests for Ada_Sqlite3
--
-- Copyright (c) 2025 Garret Noble
--
-- Licensed under the terms of the MIT License or Apache-2.0 with LLVM exception
-------------------------------------------------------------------------------

with AUnit;
with AUnit.Test_Fixtures;
with AUnit.Test_Suites;

package Statement_Tests is

   --  Test case for Statement operations
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   --  Test routines
   procedure Test_Prepare_Finalize (T : in out Test);
   procedure Test_Reset_Clear_Bindings (T : in out Test);
   procedure Test_Step (T : in out Test);
   procedure Test_Bind_Null (T : in out Test);
   procedure Test_Bind_Int (T : in out Test);
   procedure Test_Bind_Int64 (T : in out Test);
   procedure Test_Bind_Double (T : in out Test);
   procedure Test_Bind_Text (T : in out Test);
   procedure Test_Bind_Parameter_Index (T : in out Test);
   procedure Test_Column_Count (T : in out Test);
   procedure Test_Column_Name (T : in out Test);
   procedure Test_Column_Type (T : in out Test);
   procedure Test_Column_Int (T : in out Test);
   procedure Test_Column_Int64 (T : in out Test);
   procedure Test_Column_Double (T : in out Test);
   procedure Test_Column_Text (T : in out Test);
   procedure Test_Column_Is_Null (T : in out Test);
   procedure Test_Invalid_Statement (T : in out Test);

   --  Register test routines to call
   function Suite return AUnit.Test_Suites.Access_Test_Suite;

end Statement_Tests;
