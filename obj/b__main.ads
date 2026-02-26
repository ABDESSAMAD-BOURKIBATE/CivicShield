pragma Warnings (Off);
pragma Ada_95;
with System;
with System.Parameters;
with System.Secondary_Stack;
package ada_main is

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: 15.2.0" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   GNAT_Version_Address : constant System.Address := GNAT_Version'Address;
   pragma Export (C, GNAT_Version_Address, "__gnat_version_address");

   Ada_Main_Program_Name : constant String := "_ada_main" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#0b652e24#;
   pragma Export (C, u00001, "mainB");
   u00002 : constant Version_32 := 16#b2cfab41#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#ba677807#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#f9169ea9#;
   pragma Export (C, u00004, "access_controlB");
   u00005 : constant Version_32 := 16#e9babf63#;
   pragma Export (C, u00005, "access_controlS");
   u00006 : constant Version_32 := 16#76789da1#;
   pragma Export (C, u00006, "adaS");
   u00007 : constant Version_32 := 16#a201b8c5#;
   pragma Export (C, u00007, "ada__strings__text_buffersB");
   u00008 : constant Version_32 := 16#a7cfd09b#;
   pragma Export (C, u00008, "ada__strings__text_buffersS");
   u00009 : constant Version_32 := 16#e6d4fa36#;
   pragma Export (C, u00009, "ada__stringsS");
   u00010 : constant Version_32 := 16#a869df9e#;
   pragma Export (C, u00010, "systemS");
   u00011 : constant Version_32 := 16#45e1965e#;
   pragma Export (C, u00011, "system__exception_tableB");
   u00012 : constant Version_32 := 16#2542a987#;
   pragma Export (C, u00012, "system__exception_tableS");
   u00013 : constant Version_32 := 16#7fa0a598#;
   pragma Export (C, u00013, "system__soft_linksB");
   u00014 : constant Version_32 := 16#7be26ab7#;
   pragma Export (C, u00014, "system__soft_linksS");
   u00015 : constant Version_32 := 16#d0b087d0#;
   pragma Export (C, u00015, "system__secondary_stackB");
   u00016 : constant Version_32 := 16#06a28e92#;
   pragma Export (C, u00016, "system__secondary_stackS");
   u00017 : constant Version_32 := 16#ebbee607#;
   pragma Export (C, u00017, "ada__exceptionsB");
   u00018 : constant Version_32 := 16#d8988d8d#;
   pragma Export (C, u00018, "ada__exceptionsS");
   u00019 : constant Version_32 := 16#85bf25f7#;
   pragma Export (C, u00019, "ada__exceptions__last_chance_handlerB");
   u00020 : constant Version_32 := 16#a028f72d#;
   pragma Export (C, u00020, "ada__exceptions__last_chance_handlerS");
   u00021 : constant Version_32 := 16#9acc60ac#;
   pragma Export (C, u00021, "system__exceptionsS");
   u00022 : constant Version_32 := 16#c367aa24#;
   pragma Export (C, u00022, "system__exceptions__machineB");
   u00023 : constant Version_32 := 16#ec13924a#;
   pragma Export (C, u00023, "system__exceptions__machineS");
   u00024 : constant Version_32 := 16#7706238d#;
   pragma Export (C, u00024, "system__exceptions_debugB");
   u00025 : constant Version_32 := 16#986787cd#;
   pragma Export (C, u00025, "system__exceptions_debugS");
   u00026 : constant Version_32 := 16#8af69cdf#;
   pragma Export (C, u00026, "system__img_intS");
   u00027 : constant Version_32 := 16#f2c63a02#;
   pragma Export (C, u00027, "ada__numericsS");
   u00028 : constant Version_32 := 16#174f5472#;
   pragma Export (C, u00028, "ada__numerics__big_numbersS");
   u00029 : constant Version_32 := 16#5243a0c7#;
   pragma Export (C, u00029, "system__unsigned_typesS");
   u00030 : constant Version_32 := 16#64b70b76#;
   pragma Export (C, u00030, "system__storage_elementsS");
   u00031 : constant Version_32 := 16#5c7d9c20#;
   pragma Export (C, u00031, "system__tracebackB");
   u00032 : constant Version_32 := 16#2ef32b23#;
   pragma Export (C, u00032, "system__tracebackS");
   u00033 : constant Version_32 := 16#5f6b6486#;
   pragma Export (C, u00033, "system__traceback_entriesB");
   u00034 : constant Version_32 := 16#60756012#;
   pragma Export (C, u00034, "system__traceback_entriesS");
   u00035 : constant Version_32 := 16#b69e050b#;
   pragma Export (C, u00035, "system__traceback__symbolicB");
   u00036 : constant Version_32 := 16#140ceb78#;
   pragma Export (C, u00036, "system__traceback__symbolicS");
   u00037 : constant Version_32 := 16#179d7d28#;
   pragma Export (C, u00037, "ada__containersS");
   u00038 : constant Version_32 := 16#701f9d88#;
   pragma Export (C, u00038, "ada__exceptions__tracebackB");
   u00039 : constant Version_32 := 16#26ed0985#;
   pragma Export (C, u00039, "ada__exceptions__tracebackS");
   u00040 : constant Version_32 := 16#9111f9c1#;
   pragma Export (C, u00040, "interfacesS");
   u00041 : constant Version_32 := 16#401f6fd6#;
   pragma Export (C, u00041, "interfaces__cB");
   u00042 : constant Version_32 := 16#e5a34c24#;
   pragma Export (C, u00042, "interfaces__cS");
   u00043 : constant Version_32 := 16#a43efea2#;
   pragma Export (C, u00043, "system__parametersB");
   u00044 : constant Version_32 := 16#9dfe238f#;
   pragma Export (C, u00044, "system__parametersS");
   u00045 : constant Version_32 := 16#0978786d#;
   pragma Export (C, u00045, "system__bounded_stringsB");
   u00046 : constant Version_32 := 16#df94fe87#;
   pragma Export (C, u00046, "system__bounded_stringsS");
   u00047 : constant Version_32 := 16#234db811#;
   pragma Export (C, u00047, "system__crtlS");
   u00048 : constant Version_32 := 16#799f87ee#;
   pragma Export (C, u00048, "system__dwarf_linesB");
   u00049 : constant Version_32 := 16#d0240b99#;
   pragma Export (C, u00049, "system__dwarf_linesS");
   u00050 : constant Version_32 := 16#5b4659fa#;
   pragma Export (C, u00050, "ada__charactersS");
   u00051 : constant Version_32 := 16#9de61c25#;
   pragma Export (C, u00051, "ada__characters__handlingB");
   u00052 : constant Version_32 := 16#729cc5db#;
   pragma Export (C, u00052, "ada__characters__handlingS");
   u00053 : constant Version_32 := 16#cde9ea2d#;
   pragma Export (C, u00053, "ada__characters__latin_1S");
   u00054 : constant Version_32 := 16#203d5282#;
   pragma Export (C, u00054, "ada__strings__mapsB");
   u00055 : constant Version_32 := 16#6feaa257#;
   pragma Export (C, u00055, "ada__strings__mapsS");
   u00056 : constant Version_32 := 16#b451a498#;
   pragma Export (C, u00056, "system__bit_opsB");
   u00057 : constant Version_32 := 16#659a73a2#;
   pragma Export (C, u00057, "system__bit_opsS");
   u00058 : constant Version_32 := 16#b459efcb#;
   pragma Export (C, u00058, "ada__strings__maps__constantsS");
   u00059 : constant Version_32 := 16#f9910acc#;
   pragma Export (C, u00059, "system__address_imageB");
   u00060 : constant Version_32 := 16#098542a4#;
   pragma Export (C, u00060, "system__address_imageS");
   u00061 : constant Version_32 := 16#9dd7353b#;
   pragma Export (C, u00061, "system__img_address_32S");
   u00062 : constant Version_32 := 16#b0f794b9#;
   pragma Export (C, u00062, "system__img_address_64S");
   u00063 : constant Version_32 := 16#c1e0ea20#;
   pragma Export (C, u00063, "system__img_unsS");
   u00064 : constant Version_32 := 16#20ec7aa3#;
   pragma Export (C, u00064, "system__ioB");
   u00065 : constant Version_32 := 16#362b28d1#;
   pragma Export (C, u00065, "system__ioS");
   u00066 : constant Version_32 := 16#264c804d#;
   pragma Export (C, u00066, "system__mmapB");
   u00067 : constant Version_32 := 16#25542119#;
   pragma Export (C, u00067, "system__mmapS");
   u00068 : constant Version_32 := 16#367911c4#;
   pragma Export (C, u00068, "ada__io_exceptionsS");
   u00069 : constant Version_32 := 16#5102ad93#;
   pragma Export (C, u00069, "system__mmap__os_interfaceB");
   u00070 : constant Version_32 := 16#52ab6463#;
   pragma Export (C, u00070, "system__mmap__os_interfaceS");
   u00071 : constant Version_32 := 16#c04dcb27#;
   pragma Export (C, u00071, "system__os_libB");
   u00072 : constant Version_32 := 16#2d02400e#;
   pragma Export (C, u00072, "system__os_libS");
   u00073 : constant Version_32 := 16#94d23d25#;
   pragma Export (C, u00073, "system__atomic_operations__test_and_setB");
   u00074 : constant Version_32 := 16#57acee8e#;
   pragma Export (C, u00074, "system__atomic_operations__test_and_setS");
   u00075 : constant Version_32 := 16#6f0aa5bb#;
   pragma Export (C, u00075, "system__atomic_operationsS");
   u00076 : constant Version_32 := 16#553a519e#;
   pragma Export (C, u00076, "system__atomic_primitivesB");
   u00077 : constant Version_32 := 16#a0b9547d#;
   pragma Export (C, u00077, "system__atomic_primitivesS");
   u00078 : constant Version_32 := 16#b98923bf#;
   pragma Export (C, u00078, "system__case_utilB");
   u00079 : constant Version_32 := 16#677a08cb#;
   pragma Export (C, u00079, "system__case_utilS");
   u00080 : constant Version_32 := 16#256dbbe5#;
   pragma Export (C, u00080, "system__stringsB");
   u00081 : constant Version_32 := 16#33ebdf86#;
   pragma Export (C, u00081, "system__stringsS");
   u00082 : constant Version_32 := 16#836ccd31#;
   pragma Export (C, u00082, "system__object_readerB");
   u00083 : constant Version_32 := 16#a4fd4a87#;
   pragma Export (C, u00083, "system__object_readerS");
   u00084 : constant Version_32 := 16#c901dc12#;
   pragma Export (C, u00084, "system__val_lliS");
   u00085 : constant Version_32 := 16#3fcf5e91#;
   pragma Export (C, u00085, "system__val_lluS");
   u00086 : constant Version_32 := 16#fb981c03#;
   pragma Export (C, u00086, "system__sparkS");
   u00087 : constant Version_32 := 16#a571a4dc#;
   pragma Export (C, u00087, "system__spark__cut_operationsB");
   u00088 : constant Version_32 := 16#629c0fb7#;
   pragma Export (C, u00088, "system__spark__cut_operationsS");
   u00089 : constant Version_32 := 16#365e21c1#;
   pragma Export (C, u00089, "system__val_utilB");
   u00090 : constant Version_32 := 16#2bae8e00#;
   pragma Export (C, u00090, "system__val_utilS");
   u00091 : constant Version_32 := 16#382ef1e7#;
   pragma Export (C, u00091, "system__exception_tracesB");
   u00092 : constant Version_32 := 16#44f1b6f8#;
   pragma Export (C, u00092, "system__exception_tracesS");
   u00093 : constant Version_32 := 16#b65cce28#;
   pragma Export (C, u00093, "system__win32S");
   u00094 : constant Version_32 := 16#fd158a37#;
   pragma Export (C, u00094, "system__wch_conB");
   u00095 : constant Version_32 := 16#716afcfd#;
   pragma Export (C, u00095, "system__wch_conS");
   u00096 : constant Version_32 := 16#5c289972#;
   pragma Export (C, u00096, "system__wch_stwB");
   u00097 : constant Version_32 := 16#5c7bd0fc#;
   pragma Export (C, u00097, "system__wch_stwS");
   u00098 : constant Version_32 := 16#7cd63de5#;
   pragma Export (C, u00098, "system__wch_cnvB");
   u00099 : constant Version_32 := 16#77aa368d#;
   pragma Export (C, u00099, "system__wch_cnvS");
   u00100 : constant Version_32 := 16#e538de43#;
   pragma Export (C, u00100, "system__wch_jisB");
   u00101 : constant Version_32 := 16#c21d54a7#;
   pragma Export (C, u00101, "system__wch_jisS");
   u00102 : constant Version_32 := 16#0286ce9f#;
   pragma Export (C, u00102, "system__soft_links__initializeB");
   u00103 : constant Version_32 := 16#ac2e8b53#;
   pragma Export (C, u00103, "system__soft_links__initializeS");
   u00104 : constant Version_32 := 16#8599b27b#;
   pragma Export (C, u00104, "system__stack_checkingB");
   u00105 : constant Version_32 := 16#6f36ca88#;
   pragma Export (C, u00105, "system__stack_checkingS");
   u00106 : constant Version_32 := 16#8b7604c4#;
   pragma Export (C, u00106, "ada__strings__utf_encodingB");
   u00107 : constant Version_32 := 16#c9e86997#;
   pragma Export (C, u00107, "ada__strings__utf_encodingS");
   u00108 : constant Version_32 := 16#bb780f45#;
   pragma Export (C, u00108, "ada__strings__utf_encoding__stringsB");
   u00109 : constant Version_32 := 16#b85ff4b6#;
   pragma Export (C, u00109, "ada__strings__utf_encoding__stringsS");
   u00110 : constant Version_32 := 16#d1d1ed0b#;
   pragma Export (C, u00110, "ada__strings__utf_encoding__wide_stringsB");
   u00111 : constant Version_32 := 16#5678478f#;
   pragma Export (C, u00111, "ada__strings__utf_encoding__wide_stringsS");
   u00112 : constant Version_32 := 16#c2b98963#;
   pragma Export (C, u00112, "ada__strings__utf_encoding__wide_wide_stringsB");
   u00113 : constant Version_32 := 16#d7af3358#;
   pragma Export (C, u00113, "ada__strings__utf_encoding__wide_wide_stringsS");
   u00114 : constant Version_32 := 16#683e3bb7#;
   pragma Export (C, u00114, "ada__tagsB");
   u00115 : constant Version_32 := 16#4ff764f3#;
   pragma Export (C, u00115, "ada__tagsS");
   u00116 : constant Version_32 := 16#3548d972#;
   pragma Export (C, u00116, "system__htableB");
   u00117 : constant Version_32 := 16#29b08775#;
   pragma Export (C, u00117, "system__htableS");
   u00118 : constant Version_32 := 16#1f1abe38#;
   pragma Export (C, u00118, "system__string_hashB");
   u00119 : constant Version_32 := 16#8ef5070a#;
   pragma Export (C, u00119, "system__string_hashS");
   u00120 : constant Version_32 := 16#27ac21ac#;
   pragma Export (C, u00120, "ada__text_ioB");
   u00121 : constant Version_32 := 16#b8eab78e#;
   pragma Export (C, u00121, "ada__text_ioS");
   u00122 : constant Version_32 := 16#b228eb1e#;
   pragma Export (C, u00122, "ada__streamsB");
   u00123 : constant Version_32 := 16#613fe11c#;
   pragma Export (C, u00123, "ada__streamsS");
   u00124 : constant Version_32 := 16#05222263#;
   pragma Export (C, u00124, "system__put_imagesB");
   u00125 : constant Version_32 := 16#b4c7d881#;
   pragma Export (C, u00125, "system__put_imagesS");
   u00126 : constant Version_32 := 16#22b9eb9f#;
   pragma Export (C, u00126, "ada__strings__text_buffers__utilsB");
   u00127 : constant Version_32 := 16#89062ac3#;
   pragma Export (C, u00127, "ada__strings__text_buffers__utilsS");
   u00128 : constant Version_32 := 16#1cacf006#;
   pragma Export (C, u00128, "interfaces__c_streamsB");
   u00129 : constant Version_32 := 16#d07279c2#;
   pragma Export (C, u00129, "interfaces__c_streamsS");
   u00130 : constant Version_32 := 16#ec2f4d1e#;
   pragma Export (C, u00130, "system__file_ioB");
   u00131 : constant Version_32 := 16#ce268ad8#;
   pragma Export (C, u00131, "system__file_ioS");
   u00132 : constant Version_32 := 16#c34b231e#;
   pragma Export (C, u00132, "ada__finalizationS");
   u00133 : constant Version_32 := 16#d00f339c#;
   pragma Export (C, u00133, "system__finalization_rootB");
   u00134 : constant Version_32 := 16#a215e14a#;
   pragma Export (C, u00134, "system__finalization_rootS");
   u00135 : constant Version_32 := 16#ef3c5c6f#;
   pragma Export (C, u00135, "system__finalization_primitivesB");
   u00136 : constant Version_32 := 16#b52c8f67#;
   pragma Export (C, u00136, "system__finalization_primitivesS");
   u00137 : constant Version_32 := 16#3eb79f63#;
   pragma Export (C, u00137, "system__os_locksS");
   u00138 : constant Version_32 := 16#221c42f4#;
   pragma Export (C, u00138, "system__file_control_blockS");
   u00139 : constant Version_32 := 16#118cabd8#;
   pragma Export (C, u00139, "loggingB");
   u00140 : constant Version_32 := 16#446466ab#;
   pragma Export (C, u00140, "loggingS");
   u00141 : constant Version_32 := 16#ca878138#;
   pragma Export (C, u00141, "system__concat_2B");
   u00142 : constant Version_32 := 16#1d92ac69#;
   pragma Export (C, u00142, "system__concat_2S");
   u00143 : constant Version_32 := 16#3124cfea#;
   pragma Export (C, u00143, "system__concat_8B");
   u00144 : constant Version_32 := 16#25e68f5f#;
   pragma Export (C, u00144, "system__concat_8S");
   u00145 : constant Version_32 := 16#9351de22#;
   pragma Export (C, u00145, "system__taskingB");
   u00146 : constant Version_32 := 16#72282cb1#;
   pragma Export (C, u00146, "system__taskingS");
   u00147 : constant Version_32 := 16#8272aa88#;
   pragma Export (C, u00147, "system__task_primitivesS");
   u00148 : constant Version_32 := 16#518231e9#;
   pragma Export (C, u00148, "system__os_interfaceS");
   u00149 : constant Version_32 := 16#e483ae2d#;
   pragma Export (C, u00149, "interfaces__c__stringsB");
   u00150 : constant Version_32 := 16#bd4557ce#;
   pragma Export (C, u00150, "interfaces__c__stringsS");
   u00151 : constant Version_32 := 16#5bcdc5be#;
   pragma Export (C, u00151, "system__task_primitives__operationsB");
   u00152 : constant Version_32 := 16#2d30bc7b#;
   pragma Export (C, u00152, "system__task_primitives__operationsS");
   u00153 : constant Version_32 := 16#1b28662b#;
   pragma Export (C, u00153, "system__float_controlB");
   u00154 : constant Version_32 := 16#48959ca2#;
   pragma Export (C, u00154, "system__float_controlS");
   u00155 : constant Version_32 := 16#2cd40a52#;
   pragma Export (C, u00155, "system__interrupt_managementB");
   u00156 : constant Version_32 := 16#de9cb701#;
   pragma Export (C, u00156, "system__interrupt_managementS");
   u00157 : constant Version_32 := 16#73dc29bf#;
   pragma Export (C, u00157, "system__multiprocessorsB");
   u00158 : constant Version_32 := 16#90c540ed#;
   pragma Export (C, u00158, "system__multiprocessorsS");
   u00159 : constant Version_32 := 16#f169b552#;
   pragma Export (C, u00159, "system__os_primitivesB");
   u00160 : constant Version_32 := 16#af94ba68#;
   pragma Export (C, u00160, "system__os_primitivesS");
   u00161 : constant Version_32 := 16#afdc38b2#;
   pragma Export (C, u00161, "system__arith_64B");
   u00162 : constant Version_32 := 16#ecde1f4c#;
   pragma Export (C, u00162, "system__arith_64S");
   u00163 : constant Version_32 := 16#ff7f7d40#;
   pragma Export (C, u00163, "system__task_lockB");
   u00164 : constant Version_32 := 16#c9e3e8f0#;
   pragma Export (C, u00164, "system__task_lockS");
   u00165 : constant Version_32 := 16#8f947e37#;
   pragma Export (C, u00165, "system__win32__extS");
   u00166 : constant Version_32 := 16#6c8afeef#;
   pragma Export (C, u00166, "system__task_infoB");
   u00167 : constant Version_32 := 16#3f0f3330#;
   pragma Export (C, u00167, "system__task_infoS");
   u00168 : constant Version_32 := 16#3b5b4667#;
   pragma Export (C, u00168, "system__tasking__debugB");
   u00169 : constant Version_32 := 16#ac0addd7#;
   pragma Export (C, u00169, "system__tasking__debugS");
   u00170 : constant Version_32 := 16#752a67ed#;
   pragma Export (C, u00170, "system__concat_3B");
   u00171 : constant Version_32 := 16#2213c63c#;
   pragma Export (C, u00171, "system__concat_3S");
   u00172 : constant Version_32 := 16#3066cab0#;
   pragma Export (C, u00172, "system__stack_usageB");
   u00173 : constant Version_32 := 16#f629478f#;
   pragma Export (C, u00173, "system__stack_usageS");
   u00174 : constant Version_32 := 16#3938641c#;
   pragma Export (C, u00174, "system__tasking__protected_objectsB");
   u00175 : constant Version_32 := 16#94fe996c#;
   pragma Export (C, u00175, "system__tasking__protected_objectsS");
   u00176 : constant Version_32 := 16#85efc30a#;
   pragma Export (C, u00176, "system__soft_links__taskingB");
   u00177 : constant Version_32 := 16#13803e06#;
   pragma Export (C, u00177, "system__soft_links__taskingS");
   u00178 : constant Version_32 := 16#3880736e#;
   pragma Export (C, u00178, "ada__exceptions__is_null_occurrenceB");
   u00179 : constant Version_32 := 16#2f594863#;
   pragma Export (C, u00179, "ada__exceptions__is_null_occurrenceS");
   u00180 : constant Version_32 := 16#02cecc7b#;
   pragma Export (C, u00180, "system__concat_6B");
   u00181 : constant Version_32 := 16#2a3b2e96#;
   pragma Export (C, u00181, "system__concat_6S");
   u00182 : constant Version_32 := 16#0513e9ec#;
   pragma Export (C, u00182, "ada__calendar__delaysB");
   u00183 : constant Version_32 := 16#205f84f4#;
   pragma Export (C, u00183, "ada__calendar__delaysS");
   u00184 : constant Version_32 := 16#78511131#;
   pragma Export (C, u00184, "ada__calendarB");
   u00185 : constant Version_32 := 16#c907a168#;
   pragma Export (C, u00185, "ada__calendarS");
   u00186 : constant Version_32 := 16#8b26f500#;
   pragma Export (C, u00186, "cascade_failureB");
   u00187 : constant Version_32 := 16#270fd7a5#;
   pragma Export (C, u00187, "cascade_failureS");
   u00188 : constant Version_32 := 16#28a7d4d3#;
   pragma Export (C, u00188, "emergency_responseB");
   u00189 : constant Version_32 := 16#863a997b#;
   pragma Export (C, u00189, "emergency_responseS");
   u00190 : constant Version_32 := 16#d976e2b4#;
   pragma Export (C, u00190, "ada__numerics__float_randomB");
   u00191 : constant Version_32 := 16#51695213#;
   pragma Export (C, u00191, "ada__numerics__float_randomS");
   u00192 : constant Version_32 := 16#048330cd#;
   pragma Export (C, u00192, "system__random_numbersB");
   u00193 : constant Version_32 := 16#5d541f37#;
   pragma Export (C, u00193, "system__random_numbersS");
   u00194 : constant Version_32 := 16#ed5b83eb#;
   pragma Export (C, u00194, "system__random_seedB");
   u00195 : constant Version_32 := 16#38dd5d6c#;
   pragma Export (C, u00195, "system__random_seedS");
   u00196 : constant Version_32 := 16#e1e75f5b#;
   pragma Export (C, u00196, "system__val_unsS");
   u00197 : constant Version_32 := 16#ebb39bbb#;
   pragma Export (C, u00197, "system__concat_5B");
   u00198 : constant Version_32 := 16#e8f00e45#;
   pragma Export (C, u00198, "system__concat_5S");
   u00199 : constant Version_32 := 16#b7175554#;
   pragma Export (C, u00199, "healthcareB");
   u00200 : constant Version_32 := 16#fabc5a11#;
   pragma Export (C, u00200, "healthcareS");
   u00201 : constant Version_32 := 16#e1a052f1#;
   pragma Export (C, u00201, "power_gridB");
   u00202 : constant Version_32 := 16#a8cbb82d#;
   pragma Export (C, u00202, "power_gridS");
   u00203 : constant Version_32 := 16#1640d433#;
   pragma Export (C, u00203, "system__val_intS");
   u00204 : constant Version_32 := 16#3706c797#;
   pragma Export (C, u00204, "transport_controlB");
   u00205 : constant Version_32 := 16#d0143e64#;
   pragma Export (C, u00205, "transport_controlS");
   u00206 : constant Version_32 := 16#24b85af6#;
   pragma Export (C, u00206, "water_networkB");
   u00207 : constant Version_32 := 16#99fc9dbc#;
   pragma Export (C, u00207, "water_networkS");
   u00208 : constant Version_32 := 16#37c422a1#;
   pragma Export (C, u00208, "guiB");
   u00209 : constant Version_32 := 16#706ba75a#;
   pragma Export (C, u00209, "guiS");
   u00210 : constant Version_32 := 16#702c8dfe#;
   pragma Export (C, u00210, "stability_indexB");
   u00211 : constant Version_32 := 16#1edd3d25#;
   pragma Export (C, u00211, "stability_indexS");
   u00212 : constant Version_32 := 16#bcc987d2#;
   pragma Export (C, u00212, "system__concat_4B");
   u00213 : constant Version_32 := 16#9b9180a0#;
   pragma Export (C, u00213, "system__concat_4S");
   u00214 : constant Version_32 := 16#ada38524#;
   pragma Export (C, u00214, "system__concat_7B");
   u00215 : constant Version_32 := 16#a1949e01#;
   pragma Export (C, u00215, "system__concat_7S");
   u00216 : constant Version_32 := 16#63bad2e6#;
   pragma Export (C, u00216, "system__concat_9B");
   u00217 : constant Version_32 := 16#fc8617f5#;
   pragma Export (C, u00217, "system__concat_9S");
   u00218 : constant Version_32 := 16#0ddbd91f#;
   pragma Export (C, u00218, "system__memoryB");
   u00219 : constant Version_32 := 16#b0fd4384#;
   pragma Export (C, u00219, "system__memoryS");

   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.latin_1%s
   --  interfaces%s
   --  system%s
   --  system.atomic_operations%s
   --  system.float_control%s
   --  system.float_control%b
   --  system.io%s
   --  system.io%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.spark%s
   --  system.spark.cut_operations%s
   --  system.spark.cut_operations%b
   --  system.storage_elements%s
   --  system.img_address_32%s
   --  system.img_address_64%s
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%s
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  system.unsigned_types%s
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%s
   --  system.wch_cnv%b
   --  system.concat_2%s
   --  system.concat_2%b
   --  system.concat_3%s
   --  system.concat_3%b
   --  system.concat_4%s
   --  system.concat_4%b
   --  system.concat_5%s
   --  system.concat_5%b
   --  system.concat_6%s
   --  system.concat_6%b
   --  system.concat_7%s
   --  system.concat_7%b
   --  system.concat_8%s
   --  system.concat_8%b
   --  system.concat_9%s
   --  system.concat_9%b
   --  system.traceback%s
   --  system.traceback%b
   --  ada.characters.handling%s
   --  system.atomic_operations.test_and_set%s
   --  system.case_util%s
   --  system.os_lib%s
   --  system.secondary_stack%s
   --  system.standard_library%s
   --  ada.exceptions%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  system.soft_links%s
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_llu%s
   --  system.val_lli%s
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  ada.exceptions.last_chance_handler%s
   --  ada.exceptions.last_chance_handler%b
   --  ada.exceptions.traceback%s
   --  ada.exceptions.traceback%b
   --  system.address_image%s
   --  system.address_image%b
   --  system.bit_ops%s
   --  system.bit_ops%b
   --  system.bounded_strings%s
   --  system.bounded_strings%b
   --  system.case_util%b
   --  system.exception_table%s
   --  system.exception_table%b
   --  ada.containers%s
   --  ada.io_exceptions%s
   --  ada.numerics%s
   --  ada.numerics.big_numbers%s
   --  ada.strings%s
   --  ada.strings.maps%s
   --  ada.strings.maps%b
   --  ada.strings.maps.constants%s
   --  interfaces.c%s
   --  interfaces.c%b
   --  system.atomic_primitives%s
   --  system.atomic_primitives%b
   --  system.exceptions%s
   --  system.exceptions.machine%s
   --  system.exceptions.machine%b
   --  system.win32%s
   --  ada.characters.handling%b
   --  system.atomic_operations.test_and_set%b
   --  system.exception_traces%s
   --  system.exception_traces%b
   --  system.img_int%s
   --  system.img_uns%s
   --  system.memory%s
   --  system.memory%b
   --  system.mmap%s
   --  system.mmap.os_interface%s
   --  system.mmap.os_interface%b
   --  system.mmap%b
   --  system.object_reader%s
   --  system.object_reader%b
   --  system.dwarf_lines%s
   --  system.dwarf_lines%b
   --  system.os_lib%b
   --  system.secondary_stack%b
   --  system.soft_links.initialize%s
   --  system.soft_links.initialize%b
   --  system.soft_links%b
   --  system.standard_library%b
   --  system.traceback.symbolic%s
   --  system.traceback.symbolic%b
   --  ada.exceptions%b
   --  ada.exceptions.is_null_occurrence%s
   --  ada.exceptions.is_null_occurrence%b
   --  ada.strings.utf_encoding%s
   --  ada.strings.utf_encoding%b
   --  ada.strings.utf_encoding.strings%s
   --  ada.strings.utf_encoding.strings%b
   --  ada.strings.utf_encoding.wide_strings%s
   --  ada.strings.utf_encoding.wide_strings%b
   --  ada.strings.utf_encoding.wide_wide_strings%s
   --  ada.strings.utf_encoding.wide_wide_strings%b
   --  ada.tags%s
   --  ada.tags%b
   --  ada.strings.text_buffers%s
   --  ada.strings.text_buffers%b
   --  ada.strings.text_buffers.utils%s
   --  ada.strings.text_buffers.utils%b
   --  interfaces.c.strings%s
   --  interfaces.c.strings%b
   --  system.arith_64%s
   --  system.arith_64%b
   --  system.multiprocessors%s
   --  system.multiprocessors%b
   --  system.os_locks%s
   --  system.finalization_primitives%s
   --  system.finalization_primitives%b
   --  system.os_interface%s
   --  system.interrupt_management%s
   --  system.interrupt_management%b
   --  system.put_images%s
   --  system.put_images%b
   --  ada.streams%s
   --  ada.streams%b
   --  system.file_control_block%s
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  system.file_io%s
   --  system.file_io%b
   --  system.stack_usage%s
   --  system.stack_usage%b
   --  system.task_info%s
   --  system.task_info%b
   --  system.task_lock%s
   --  system.task_lock%b
   --  system.task_primitives%s
   --  system.val_uns%s
   --  system.val_int%s
   --  system.win32.ext%s
   --  system.os_primitives%s
   --  system.os_primitives%b
   --  system.tasking%s
   --  system.task_primitives.operations%s
   --  system.tasking.debug%s
   --  system.tasking.debug%b
   --  system.task_primitives.operations%b
   --  system.tasking%b
   --  ada.calendar%s
   --  ada.calendar%b
   --  ada.calendar.delays%s
   --  ada.calendar.delays%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  system.random_seed%s
   --  system.random_seed%b
   --  system.random_numbers%s
   --  system.random_numbers%b
   --  ada.numerics.float_random%s
   --  ada.numerics.float_random%b
   --  system.soft_links.tasking%s
   --  system.soft_links.tasking%b
   --  system.tasking.protected_objects%s
   --  system.tasking.protected_objects%b
   --  logging%s
   --  logging%b
   --  access_control%s
   --  access_control%b
   --  emergency_response%s
   --  emergency_response%b
   --  healthcare%s
   --  healthcare%b
   --  power_grid%s
   --  power_grid%b
   --  stability_index%s
   --  stability_index%b
   --  transport_control%s
   --  transport_control%b
   --  water_network%s
   --  water_network%b
   --  cascade_failure%s
   --  cascade_failure%b
   --  gui%s
   --  gui%b
   --  main%b
   --  END ELABORATION ORDER

end ada_main;
