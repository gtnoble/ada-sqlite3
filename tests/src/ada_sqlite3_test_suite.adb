-------------------------------------------------------------------------------
-- Ada_Sqlite3 Test Suite
--
-- Copyright (c) 2025 Garret Noble
--
-- Licensed under the terms of the MIT License or Apache-2.0 with LLVM exception
-------------------------------------------------------------------------------

with AUnit.Test_Suites;
with Database_Tests;
with Statement_Tests;
with Blob_Tests;
with Transaction_Tests;

package body Ada_Sqlite3_Test_Suite is

   use AUnit.Test_Suites;

   --  Instantiate the test suites
   function Suite return Access_Test_Suite is
      Result : constant Access_Test_Suite := new Test_Suite;
   begin
      --  Add the test cases to the suite
      Result.Add_Test (Database_Tests.Suite);
      Result.Add_Test (Statement_Tests.Suite);
      --  Result.Add_Test (Blob_Tests.Suite);
      Result.Add_Test (Transaction_Tests.Suite);
      
      return Result;
   end Suite;

end Ada_Sqlite3_Test_Suite;
