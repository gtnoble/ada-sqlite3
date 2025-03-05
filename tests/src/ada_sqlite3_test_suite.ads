-------------------------------------------------------------------------------
-- Ada_Sqlite3 Test Suite
--
-- Copyright (c) 2025 Garret Noble
--
-- Licensed under the terms of the MIT License or Apache-2.0 with LLVM exception
-------------------------------------------------------------------------------

with AUnit.Test_Suites;

package Ada_Sqlite3_Test_Suite is

   --  Return the test suite
   function Suite return AUnit.Test_Suites.Access_Test_Suite;

end Ada_Sqlite3_Test_Suite;
