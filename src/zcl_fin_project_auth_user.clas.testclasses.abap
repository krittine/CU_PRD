*"* use this source file for your ABAP unit test classes

CLASS tcl_test_tracer DEFINITION .
  PUBLIC SECTION.
    INTERFACES if_fin_re_message_tracer .
  PRIVATE SECTION.
    DATA: mt_messages TYPE STANDARD TABLE OF if_fin_re_message_tracer=>t_message .
ENDCLASS.

CLASS tcl_test_tracer IMPLEMENTATION.

  METHOD if_fin_re_message_tracer~trace.
    APPEND is_message TO mt_messages.
  ENDMETHOD.

ENDCLASS.

CLASS tcl_test_subval_function DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
    METHODS:
      test_execution  FOR TESTING,
      test_rule_check FOR TESTING,
      test_signature  FOR TESTING,
      test_enabling   FOR TESTING.


ENDCLASS.

CLASS tcl_test_subval_function IMPLEMENTATION.

  METHOD test_execution.
    DATA: lt_gets      TYPE STANDARD TABLE OF zcl_fin_project_auth_user=>t_get,
          lo_function  TYPE REF TO if_fin_re_custom_function,
          lo_tracer    TYPE REF TO if_fin_re_message_tracer,
          lv_user      TYPE zduname.

*    lt_weekdays =  VALUE #( ( zcl_project_budget=>c_monday    )
*                            ( zcl_project_budget=>c_tuesday   )
*                            ( zcl_project_budget=>c_wednesday )
*                            ( zcl_project_budget=>c_thursday  )
*                            ( zcl_project_budget=>c_friday    )
*                            ( zcl_project_budget=>c_saterday  )
*                            ( zcl_project_budget=>c_sunday    ) ).

    lo_function = NEW zcl_fin_project_auth_user( ).

    cl_abap_unit_assert=>assert_equals( act =  lo_function->get_name( ) exp = zcl_fin_project_auth_user=>c_function_name ) .
    lo_tracer = NEW tcl_test_tracer( ).

    " check 7 weekdays
    lv_user = 'CB9980000013'. " Krittin
    "DO 7 TIMES.
      "lv_test_date = lv_test_date + 1.
      TRY.
          lo_function->execute(
            EXPORTING
              is_runtime = VALUE #( event_id   = 'FINS_ACC_MS_1'
                                    rule_id    = 'DummyRuleId'
                                    rule_name  = 'ZDummyRuleName'
                                    tracer     = lo_tracer
                                    parameters = VALUE #( ( name = zcl_fin_project_auth_user=>c_parameter_name value = REF #( lv_user ) ) )
               )
            IMPORTING
              ev_result  = DATA(lr_result)
          ).
        CATCH cx_fin_re_exception.
          cl_abap_unit_assert=>fail( ).
      ENDTRY.
*      cl_abap_unit_assert=>assert_equals( act =  lr_result->* exp = lt_weekdays[ sy-index ] ) .
    "ENDDO.

    " check unbound input value
    CLEAR: lr_result.
    TRY.
        lo_function->execute(
          EXPORTING
            is_runtime = VALUE #( event_id   = 'FINS_ACC_MS_1'
                                  rule_id    = 'DummyRuleId'
                                  rule_name  = 'ZDummyRuleName'
                                  tracer     = lo_tracer
                                  parameters = VALUE #( ( name = zcl_fin_project_auth_user=>c_parameter_name  ) )
             )
          IMPORTING
            ev_result  = lr_result
        ).
      CATCH cx_fin_re_exception.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.
    "cl_abap_unit_assert=>assert_not_bound( act = lr_result ).

    " check invalid char
    lv_user = 'CB9980000013'. " Krittin
    TRY.
        lo_function->execute(
          EXPORTING
            is_runtime = VALUE #( event_id   = 'FINS_ACC_MS_1'
                                  rule_id    = 'DummyRuleId'
                                  rule_name  = 'ZDummyRuleName'
                                  tracer     = lo_tracer
                                  parameters = VALUE #( ( name = zcl_fin_project_auth_user=>c_parameter_name value = REF #( lv_user ) ) )
             )
          IMPORTING
            ev_result  = lr_result
        ).
      CATCH cx_fin_re_exception.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.
    "cl_abap_unit_assert=>assert_not_bound( act = lr_result ).

    CLEAR lv_user.
    TRY.
        lo_function->execute(
          EXPORTING
            is_runtime = VALUE #( event_id   = 'FINS_ACC_MS_1'
                                  rule_id    = 'DummyRuleId'
                                  rule_name  = 'ZDummyRuleName'
                                  tracer     = lo_tracer
                                  parameters = VALUE #( ( name = zcl_fin_project_auth_user=>c_parameter_name value = REF #( lv_user ) ) )
             )
          IMPORTING
            ev_result  = lr_result
        ).
      CATCH cx_fin_re_exception.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.
    "cl_abap_unit_assert=>assert_not_bound( act = lr_result ).



  ENDMETHOD.

  METHOD test_rule_check.
    DATA:
      lo_function TYPE REF TO if_fin_re_custom_function,
      lo_tracer   TYPE REF TO if_fin_re_message_tracer.

    lo_function = NEW zcl_fin_project_auth_user( ).
    lo_tracer   = NEW tcl_test_tracer( ).
    lo_function->check_at_rule_activation(
      EXPORTING
        is_controlblock = VALUE #( event_id   = 'FINS_ACC_MS_1'
                                   rule_id    = 'DummyRuleId'
                                   rule_name  = 'ZDummyRuleName'
                                   tracer     = lo_tracer
                                   parameters = VALUE #( ( name       =  zcl_fin_project_auth_user=>c_parameter_name
                                                           expression = VALUE #(
                                                           abap_type  =  CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_name( 'ZDUNAME' ) ) ) ) ) )
      IMPORTING
        ev_rc           = DATA(lv_rc)
    ).
    cl_abap_unit_assert=>assert_equals( act = lv_rc exp = if_fin_re_custom_function=>rc-ok ) .
    lo_function->check_at_rule_activation(
     EXPORTING
       is_controlblock = VALUE #( event_id   = 'FINS_ACC_MS_1'
                                  rule_id    = 'DummyRuleId'
                                  rule_name  = 'ZDummyRuleName'
                                  tracer     = lo_tracer
                                  parameters = VALUE #( ( name       = zcl_fin_project_auth_user=>c_parameter_name
                                                          expression = VALUE #(
                                                          abap_type  = cl_abap_elemdescr=>get_c( 8 ) ) ) ) )
     IMPORTING
       ev_rc           = lv_rc
   ).
    "cl_abap_unit_assert=>assert_equals( act = lv_rc exp = if_fin_re_custom_function=>rc-error ) .
  ENDMETHOD.

  METHOD test_signature.
    DATA:
         lo_function TYPE REF TO if_fin_re_custom_function.
    lo_function = NEW zcl_fin_project_auth_user( ).

    "cl_abap_unit_assert=>assert_equals( act = lo_function->get_description( )
    "                                    exp = VALUE symsg( msgid = 'ZMPROJ' msgno = '003' ) ).

    cl_abap_unit_assert=>assert_not_initial(  lo_function->get_returntype( ) ).

    DATA(lt_params) = lo_function->get_parameters( ).
    cl_abap_unit_assert=>assert_equals( act = xsdbool( line_exists( lt_params[ KEY p COMPONENTS name = zcl_fin_project_auth_user=>c_parameter_name ] ) ) exp = abap_true ).

  ENDMETHOD.

  METHOD test_enabling.

    DATA: lo_function TYPE REF TO if_fin_re_custom_function.
    lo_function = NEW zcl_fin_project_auth_user( ).
    "cl_abap_unit_assert=>assert_equals( act = lo_function->is_disabled( ) exp = abap_true ).
    "cl_abap_unit_assert=>assert_equals( act = lo_function->is_disabled( iv_event_id = 'FINS_ACC_MS_1' ) exp = abap_true ).

  ENDMETHOD.

ENDCLASS.
