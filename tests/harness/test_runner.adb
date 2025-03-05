-------------------------------------------------------------------------------
-- Ada_Sqlite3 Test Runner
--
-- Copyright (c) 2025 Garret Noble
--
-- Licensed under the terms of the MIT License or Apache-2.0 with LLVM exception
-------------------------------------------------------------------------------

with AUnit.Reporter.Text;
with AUnit.Run;
with Ada_Sqlite3_Test_Suite;

procedure Test_Runner is
   
   --  Create a test runner
   procedure Run is new AUnit.Run.Test_Runner (Ada_Sqlite3_Test_Suite.Suite);
   
   --  Create a text reporter
   Reporter : AUnit.Reporter.Text.Text_Reporter;
   
begin
   --  Run the tests
   Run (Reporter);
end Test_Runner;
