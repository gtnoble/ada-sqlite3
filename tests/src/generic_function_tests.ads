with AUnit;
with AUnit.Test_Fixtures;
with AUnit.Test_Suites;

package Generic_Function_Tests is
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private
   procedure Test_Scalar_Function (T : in out Test);
   procedure Test_Aggregate_Function (T : in out Test);
   procedure Test_Window_Function (T : in out Test);
   procedure Test_UTF16_Text (T : in out Test);
   procedure Test_Context_Cleanup (T : in out Test);
   procedure Test_Multiple_Functions (T : in out Test);
end Generic_Function_Tests;
