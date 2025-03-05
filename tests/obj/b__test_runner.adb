pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__test_runner.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__test_runner.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body ada_main is

   E069 : Short_Integer; pragma Import (Ada, E069, "system__os_lib_E");
   E016 : Short_Integer; pragma Import (Ada, E016, "ada__exceptions_E");
   E012 : Short_Integer; pragma Import (Ada, E012, "system__soft_links_E");
   E010 : Short_Integer; pragma Import (Ada, E010, "system__exception_table_E");
   E035 : Short_Integer; pragma Import (Ada, E035, "ada__containers_E");
   E064 : Short_Integer; pragma Import (Ada, E064, "ada__io_exceptions_E");
   E025 : Short_Integer; pragma Import (Ada, E025, "ada__numerics_E");
   E007 : Short_Integer; pragma Import (Ada, E007, "ada__strings_E");
   E053 : Short_Integer; pragma Import (Ada, E053, "ada__strings__maps_E");
   E056 : Short_Integer; pragma Import (Ada, E056, "ada__strings__maps__constants_E");
   E040 : Short_Integer; pragma Import (Ada, E040, "interfaces__c_E");
   E019 : Short_Integer; pragma Import (Ada, E019, "system__exceptions_E");
   E080 : Short_Integer; pragma Import (Ada, E080, "system__object_reader_E");
   E047 : Short_Integer; pragma Import (Ada, E047, "system__dwarf_lines_E");
   E099 : Short_Integer; pragma Import (Ada, E099, "system__soft_links__initialize_E");
   E034 : Short_Integer; pragma Import (Ada, E034, "system__traceback__symbolic_E");
   E103 : Short_Integer; pragma Import (Ada, E103, "ada__strings__utf_encoding_E");
   E111 : Short_Integer; pragma Import (Ada, E111, "ada__tags_E");
   E005 : Short_Integer; pragma Import (Ada, E005, "ada__strings__text_buffers_E");
   E190 : Short_Integer; pragma Import (Ada, E190, "gnat_E");
   E202 : Short_Integer; pragma Import (Ada, E202, "interfaces__c__strings_E");
   E154 : Short_Integer; pragma Import (Ada, E154, "ada__streams_E");
   E166 : Short_Integer; pragma Import (Ada, E166, "system__file_control_block_E");
   E165 : Short_Integer; pragma Import (Ada, E165, "system__finalization_root_E");
   E163 : Short_Integer; pragma Import (Ada, E163, "ada__finalization_E");
   E162 : Short_Integer; pragma Import (Ada, E162, "system__file_io_E");
   E185 : Short_Integer; pragma Import (Ada, E185, "system__storage_pools_E");
   E183 : Short_Integer; pragma Import (Ada, E183, "system__finalization_masters_E");
   E222 : Short_Integer; pragma Import (Ada, E222, "system__storage_pools__subpools_E");
   E238 : Short_Integer; pragma Import (Ada, E238, "ada__strings__unbounded_E");
   E145 : Short_Integer; pragma Import (Ada, E145, "ada__calendar_E");
   E152 : Short_Integer; pragma Import (Ada, E152, "ada__text_io_E");
   E187 : Short_Integer; pragma Import (Ada, E187, "system__pool_global_E");
   E246 : Short_Integer; pragma Import (Ada, E246, "system__regexp_E");
   E228 : Short_Integer; pragma Import (Ada, E228, "ada__directories_E");
   E198 : Short_Integer; pragma Import (Ada, E198, "ada_sqlite3_E");
   E200 : Short_Integer; pragma Import (Ada, E200, "ada_sqlite3__low_level_E");
   E206 : Short_Integer; pragma Import (Ada, E206, "ada_sqlite3__blobs_E");
   E119 : Short_Integer; pragma Import (Ada, E119, "aunit_E");
   E121 : Short_Integer; pragma Import (Ada, E121, "aunit__memory_E");
   E128 : Short_Integer; pragma Import (Ada, E128, "aunit__memory__utils_E");
   E125 : Short_Integer; pragma Import (Ada, E125, "ada_containers__aunit_lists_E");
   E181 : Short_Integer; pragma Import (Ada, E181, "aunit__tests_E");
   E139 : Short_Integer; pragma Import (Ada, E139, "aunit__time_measure_E");
   E137 : Short_Integer; pragma Import (Ada, E137, "aunit__test_results_E");
   E135 : Short_Integer; pragma Import (Ada, E135, "aunit__assertions_E");
   E131 : Short_Integer; pragma Import (Ada, E131, "aunit__test_filters_E");
   E133 : Short_Integer; pragma Import (Ada, E133, "aunit__simple_test_cases_E");
   E252 : Short_Integer; pragma Import (Ada, E252, "aunit__reporter_E");
   E254 : Short_Integer; pragma Import (Ada, E254, "aunit__reporter__text_E");
   E216 : Short_Integer; pragma Import (Ada, E216, "aunit__test_fixtures_E");
   E214 : Short_Integer; pragma Import (Ada, E214, "aunit__test_caller_E");
   E123 : Short_Integer; pragma Import (Ada, E123, "aunit__test_suites_E");
   E259 : Short_Integer; pragma Import (Ada, E259, "aunit__run_E");
   E196 : Short_Integer; pragma Import (Ada, E196, "blob_tests_E");
   E226 : Short_Integer; pragma Import (Ada, E226, "database_tests_E");
   E248 : Short_Integer; pragma Import (Ada, E248, "statement_tests_E");
   E250 : Short_Integer; pragma Import (Ada, E250, "transaction_tests_E");
   E117 : Short_Integer; pragma Import (Ada, E117, "ada_sqlite3_test_suite_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      declare
         procedure F1;
         pragma Import (Ada, F1, "transaction_tests__finalize_body");
      begin
         E250 := E250 - 1;
         F1;
      end;
      declare
         procedure F2;
         pragma Import (Ada, F2, "transaction_tests__finalize_spec");
      begin
         F2;
      end;
      declare
         procedure F3;
         pragma Import (Ada, F3, "statement_tests__finalize_body");
      begin
         E248 := E248 - 1;
         F3;
      end;
      declare
         procedure F4;
         pragma Import (Ada, F4, "statement_tests__finalize_spec");
      begin
         F4;
      end;
      declare
         procedure F5;
         pragma Import (Ada, F5, "database_tests__finalize_body");
      begin
         E226 := E226 - 1;
         F5;
      end;
      declare
         procedure F6;
         pragma Import (Ada, F6, "database_tests__finalize_spec");
      begin
         F6;
      end;
      declare
         procedure F7;
         pragma Import (Ada, F7, "blob_tests__finalize_body");
      begin
         E196 := E196 - 1;
         F7;
      end;
      declare
         procedure F8;
         pragma Import (Ada, F8, "blob_tests__finalize_spec");
      begin
         F8;
      end;
      E123 := E123 - 1;
      declare
         procedure F9;
         pragma Import (Ada, F9, "aunit__test_suites__finalize_spec");
      begin
         F9;
      end;
      E216 := E216 - 1;
      declare
         procedure F10;
         pragma Import (Ada, F10, "aunit__test_fixtures__finalize_spec");
      begin
         F10;
      end;
      E254 := E254 - 1;
      declare
         procedure F11;
         pragma Import (Ada, F11, "aunit__reporter__text__finalize_spec");
      begin
         F11;
      end;
      E131 := E131 - 1;
      E133 := E133 - 1;
      declare
         procedure F12;
         pragma Import (Ada, F12, "aunit__simple_test_cases__finalize_spec");
      begin
         F12;
      end;
      declare
         procedure F13;
         pragma Import (Ada, F13, "aunit__test_filters__finalize_spec");
      begin
         F13;
      end;
      E135 := E135 - 1;
      declare
         procedure F14;
         pragma Import (Ada, F14, "aunit__assertions__finalize_spec");
      begin
         F14;
      end;
      E137 := E137 - 1;
      declare
         procedure F15;
         pragma Import (Ada, F15, "aunit__test_results__finalize_spec");
      begin
         F15;
      end;
      declare
         procedure F16;
         pragma Import (Ada, F16, "aunit__tests__finalize_spec");
      begin
         E181 := E181 - 1;
         F16;
      end;
      E206 := E206 - 1;
      declare
         procedure F17;
         pragma Import (Ada, F17, "ada_sqlite3__blobs__finalize_spec");
      begin
         F17;
      end;
      E198 := E198 - 1;
      declare
         procedure F18;
         pragma Import (Ada, F18, "ada_sqlite3__finalize_spec");
      begin
         F18;
      end;
      declare
         procedure F19;
         pragma Import (Ada, F19, "ada__directories__finalize_body");
      begin
         E228 := E228 - 1;
         F19;
      end;
      declare
         procedure F20;
         pragma Import (Ada, F20, "ada__directories__finalize_spec");
      begin
         F20;
      end;
      E246 := E246 - 1;
      declare
         procedure F21;
         pragma Import (Ada, F21, "system__regexp__finalize_spec");
      begin
         F21;
      end;
      E187 := E187 - 1;
      declare
         procedure F22;
         pragma Import (Ada, F22, "system__pool_global__finalize_spec");
      begin
         F22;
      end;
      E152 := E152 - 1;
      declare
         procedure F23;
         pragma Import (Ada, F23, "ada__text_io__finalize_spec");
      begin
         F23;
      end;
      E238 := E238 - 1;
      declare
         procedure F24;
         pragma Import (Ada, F24, "ada__strings__unbounded__finalize_spec");
      begin
         F24;
      end;
      E222 := E222 - 1;
      declare
         procedure F25;
         pragma Import (Ada, F25, "system__storage_pools__subpools__finalize_spec");
      begin
         F25;
      end;
      E183 := E183 - 1;
      declare
         procedure F26;
         pragma Import (Ada, F26, "system__finalization_masters__finalize_spec");
      begin
         F26;
      end;
      declare
         procedure F27;
         pragma Import (Ada, F27, "system__file_io__finalize_body");
      begin
         E162 := E162 - 1;
         F27;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (Ada, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;
   pragma Favor_Top_Level (No_Param_Proc);

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Exception_Tracebacks : Integer;
      pragma Import (C, Exception_Tracebacks, "__gl_exception_tracebacks");
      Exception_Tracebacks_Symbolic : Integer;
      pragma Import (C, Exception_Tracebacks_Symbolic, "__gl_exception_tracebacks_symbolic");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Default_Secondary_Stack_Size : System.Parameters.Size_Type;
      pragma Import (C, Default_Secondary_Stack_Size, "__gnat_default_ss_size");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
      Binder_Sec_Stacks_Count : Natural;
      pragma Import (Ada, Binder_Sec_Stacks_Count, "__gnat_binder_ss_count");
      Default_Sized_SS_Pool : System.Address;
      pragma Import (Ada, Default_Sized_SS_Pool, "__gnat_default_ss_pool");

   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Exception_Tracebacks := 1;
      Exception_Tracebacks_Symbolic := 1;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;

      ada_main'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;

      Runtime_Initialize (1);

      Finalize_Library_Objects := finalize_library'access;

      Ada.Exceptions'Elab_Spec;
      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E010 := E010 + 1;
      Ada.Containers'Elab_Spec;
      E035 := E035 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E064 := E064 + 1;
      Ada.Numerics'Elab_Spec;
      E025 := E025 + 1;
      Ada.Strings'Elab_Spec;
      E007 := E007 + 1;
      Ada.Strings.Maps'Elab_Spec;
      E053 := E053 + 1;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E056 := E056 + 1;
      Interfaces.C'Elab_Spec;
      E040 := E040 + 1;
      System.Exceptions'Elab_Spec;
      E019 := E019 + 1;
      System.Object_Reader'Elab_Spec;
      E080 := E080 + 1;
      System.Dwarf_Lines'Elab_Spec;
      E047 := E047 + 1;
      System.Os_Lib'Elab_Body;
      E069 := E069 + 1;
      System.Soft_Links.Initialize'Elab_Body;
      E099 := E099 + 1;
      E012 := E012 + 1;
      System.Traceback.Symbolic'Elab_Body;
      E034 := E034 + 1;
      E016 := E016 + 1;
      Ada.Strings.Utf_Encoding'Elab_Spec;
      E103 := E103 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E111 := E111 + 1;
      Ada.Strings.Text_Buffers'Elab_Spec;
      E005 := E005 + 1;
      Gnat'Elab_Spec;
      E190 := E190 + 1;
      Interfaces.C.Strings'Elab_Spec;
      E202 := E202 + 1;
      Ada.Streams'Elab_Spec;
      E154 := E154 + 1;
      System.File_Control_Block'Elab_Spec;
      E166 := E166 + 1;
      System.Finalization_Root'Elab_Spec;
      E165 := E165 + 1;
      Ada.Finalization'Elab_Spec;
      E163 := E163 + 1;
      System.File_Io'Elab_Body;
      E162 := E162 + 1;
      System.Storage_Pools'Elab_Spec;
      E185 := E185 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E183 := E183 + 1;
      System.Storage_Pools.Subpools'Elab_Spec;
      E222 := E222 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E238 := E238 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E145 := E145 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E152 := E152 + 1;
      System.Pool_Global'Elab_Spec;
      E187 := E187 + 1;
      System.Regexp'Elab_Spec;
      E246 := E246 + 1;
      Ada.Directories'Elab_Spec;
      Ada.Directories'Elab_Body;
      E228 := E228 + 1;
      Ada_Sqlite3'Elab_Spec;
      E200 := E200 + 1;
      Ada_Sqlite3'Elab_Body;
      E198 := E198 + 1;
      Ada_Sqlite3.Blobs'Elab_Spec;
      Ada_Sqlite3.Blobs'Elab_Body;
      E206 := E206 + 1;
      E121 := E121 + 1;
      E119 := E119 + 1;
      E128 := E128 + 1;
      E125 := E125 + 1;
      Aunit.Tests'Elab_Spec;
      E181 := E181 + 1;
      Aunit.Time_Measure'Elab_Spec;
      E139 := E139 + 1;
      Aunit.Test_Results'Elab_Spec;
      E137 := E137 + 1;
      Aunit.Assertions'Elab_Spec;
      Aunit.Assertions'Elab_Body;
      E135 := E135 + 1;
      Aunit.Test_Filters'Elab_Spec;
      Aunit.Simple_Test_Cases'Elab_Spec;
      E133 := E133 + 1;
      E131 := E131 + 1;
      Aunit.Reporter'Elab_Spec;
      E252 := E252 + 1;
      Aunit.Reporter.Text'Elab_Spec;
      E254 := E254 + 1;
      Aunit.Test_Fixtures'Elab_Spec;
      E216 := E216 + 1;
      E214 := E214 + 1;
      Aunit.Test_Suites'Elab_Spec;
      E123 := E123 + 1;
      E259 := E259 + 1;
      Blob_Tests'Elab_Spec;
      Blob_Tests'Elab_Body;
      E196 := E196 + 1;
      Database_Tests'Elab_Spec;
      Database_Tests'Elab_Body;
      E226 := E226 + 1;
      Statement_Tests'Elab_Spec;
      Statement_Tests'Elab_Body;
      E248 := E248 + 1;
      Transaction_Tests'Elab_Spec;
      Transaction_Tests'Elab_Body;
      E250 := E250 + 1;
      E117 := E117 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_test_runner");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      if gnat_argc = 0 then
         gnat_argc := argc;
         gnat_argv := argv;
      end if;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   /home/gtnoble/Documents/Projects/ada-sqlite3/tests/obj/blob_tests.o
   --   /home/gtnoble/Documents/Projects/ada-sqlite3/tests/obj/database_tests.o
   --   /home/gtnoble/Documents/Projects/ada-sqlite3/tests/obj/statement_tests.o
   --   /home/gtnoble/Documents/Projects/ada-sqlite3/tests/obj/transaction_tests.o
   --   /home/gtnoble/Documents/Projects/ada-sqlite3/tests/obj/ada_sqlite3_test_suite.o
   --   /home/gtnoble/Documents/Projects/ada-sqlite3/tests/obj/test_runner.o
   --   -L/home/gtnoble/Documents/Projects/ada-sqlite3/tests/obj/
   --   -L/home/gtnoble/Documents/Projects/ada-sqlite3/tests/obj/
   --   -L/home/gtnoble/Documents/Projects/ada-sqlite3/lib/
   --   -L/home/gtnoble/.local/share/alire/builds/aunit_25.0.0_3882c581/6611d925c6ad71fb03c1f1c7dedf0728fd3ecf20865152adbd9d7d9a20c35706/lib/aunit/native-full/
   --   -L/home/gtnoble/.local/share/alire/toolchains/gnat_native_14.2.1_06bb3def/lib/gcc/x86_64-pc-linux-gnu/14.2.0/adalib/
   --   -static
   --   -lgnat
   --   -ldl
--  END Object file/option list   

end ada_main;
