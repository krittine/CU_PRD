"! <h1>Demo Implementation of Custom Function Interface</h1>
"! <p>This class offers a demo implementation of the interface {@link IF_FIN_RE_CUSTOM_FUNCTION}.<br/> Implementing this interface allows you to integrate custom ABAP logic into the <em>Manage Substitution/Validation Rules</em> app.
"! Once the implementing ABAP class is activated, the implementation is made available as a function inside the <em>Manage Substitution/Validation Rules</em> app,
"! which can be integrated into the custom substitution/validation rules.</p>
"! <p>This demo implementation is intentionally not visible by default, since the method if_fin_re_custom_function~is_disabled down below always returns true.
"! But it can be used as a starting point to implement custom functions in the <em>SAP S/4HANA Cloud ABAP Environment</em>.</p>
"! <p>Since this is simply a demo implementation, the actual logic provided here is very simple: a weekday calculation.
"! But instead of this demo logic any arbitrary functional ABAP logic could be wrapped inside such a class, and thus, made available
"! in the <em>Manage Substitution/Validation Rules</em> app.</p>
CLASS zcl_fin_project_auth_user DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    " Marker interface
    INTERFACES if_fin_re_custom_function .


  PRIVATE SECTION.
    " Return type of the function
    TYPES: t_get TYPE c LENGTH 2.


    CONSTANTS:
      "! <p>Constant definitions that are used by the implementation.</p>
      "! <p><em>To Do</em>: Obviously, this can and should be removed when implementing other functions.
      "! <p><em>Note</em>: The name of the function has to start with either 'Z', 'Y', or a customer name space.
      "! The function has to use the same name space as the implementing ABAP class.</p>
      c_function_name  TYPE fin_re_custom_function_name VALUE 'ZGET_USER',
      c_parameter_name TYPE c LENGTH 4                  VALUE 'USER'.
ENDCLASS.



CLASS ZCL_FIN_PROJECT_AUTH_USER IMPLEMENTATION.


  METHOD if_fin_re_custom_function~check_at_rule_activation.
    " This method can be used to add additional checks which are executed when a substitution/validation rule using this function
    " is activated. By implementing this function, it can be checked that only values of a specific type
    " are passed to the function, for example. This is displayed below:
    "  In this case, only values of type DATE are allowed as a parameter of the weekday calculation.
    "  If any other type of values are passed to the function, the substitution/validation rule cannot be activated.
    " In the same way, other checks are possible as well. It could, for example, be checked that only specific source fields are passed to the function
    " and not arbitrary values, or only constant values are allowed as parameters.

    " TODO: Adapt the following coding according to the requirements of your use case.
    "IF is_controlblock-parameters[ KEY p COMPONENTS name = c_parameter_name ]-expression-abap_type->type_kind <> cl_abap_typedescr=>typekind_date.
    IF is_controlblock-parameters[ KEY p COMPONENTS name = c_parameter_name ]-expression-abap_type->type_kind <> cl_abap_typedescr=>typekind_char.
      is_controlblock-tracer->trace(  VALUE #( msgid = 'ZFIN' msgno = '004' ) ). "msgv1 = 'Only Values of type CHAR allowed'
      ev_rc = if_fin_re_custom_function=>rc-error.
    ENDIF.
  ENDMETHOD.


  METHOD if_fin_re_custom_function~execute.
    " Method to actually implement the logic of the function at hand.
    " The importing parameter is_runtime contains
    "     - The actual parameters and their runtime values
    "     - A reference to a tracer object
    "     - The executed event and rule ID
    "     - The name of the executed rule
    "
    "   As shown in the demo code below, the actual parameter values can be accessed by
    "   fetching the corresponding parameter entry from the internal table is_runtime-parameters
    "   and dereferencing the contained attribute VALUE of type REF TO DATA.
    "   Please note, this reference can also be unbound. If the parameter value is retrieved via a path expression
    "   using CDS view associations, the generated database select could return an empty result. In this case, the
    "   parameter value is an unbound reference.
    "   Likewise, the exporting result of the function call is a reference of type REF TO DATA. If for whatever reason,
    "   your function cannot return a valid value, please simply leave EV_RESULT unbound.
    "   Furthermore, your function can raise an exception of type CX_FIN_RE_EXCEPTION. In this case, the
    "   Manage Substitution/Validation Rules app aborts the rule execution and returns an error code to the calling application.
    "
    "   TODO: Since this is simply a demo implementation, please replace this code in your class.
*    READ TABLE is_runtime-parameters WITH KEY p COMPONENTS name = c_parameter_name INTO DATA(ls_date).
*    IF ls_date-value IS BOUND.
*      "  data: lv_dummy type I_GLAccountLineItem-yy1_weekday_mse.
*      DATA(lv_date) = CONV dats( ls_date-value->* ).
*      IF lv_date IS NOT INITIAL AND lv_date <> '99991231'.
*        data(lv_dummy) = ( lv_date - c_known_monday ).
*        DATA(wd) = ( lv_date - c_known_monday ) MOD 7 .
*        ev_result  = COND #( WHEN wd = 0 THEN REF #( c_monday    )
*                             WHEN wd = 1 THEN REF #( c_tuesday   )
*                             WHEN wd = 2 THEN REF #( c_wednesday )
*                             WHEN wd = 3 THEN REF #( c_thursday  )
*                             WHEN wd = 4 THEN REF #( c_friday    )
*                             WHEN wd = 5 THEN REF #( c_saterday  )
*                                         ELSE REF #( c_sunday    ) ).
*      ENDIF.
*    ENDIF.
    DATA: ls_user    TYPE ztfin_user,
          lv_user    TYPE c LENGTH 12,
          lv_date    TYPE datn,
          lv_date_fr TYPE datn,
          lv_date_to TYPE datn.
    DATA: time_stamp TYPE timestamp,
          tz         TYPE tznzone,
          temp       TYPE c LENGTH 5.
    READ TABLE is_runtime-parameters WITH KEY p COMPONENTS name = c_parameter_name INTO DATA(ls_data).
    IF ls_data-value IS BOUND.
      "lv_user = CONV #( ls_data-value->* ).

      lv_user = sy-uname.
      SELECT FROM ztfin_user WITH PRIVILEGED ACCESS
        FIELDS
        authuser,
        valid_fr,
        valid_to,
        name,
        ' '        AS flag
        WHERE authuser = @lv_user
        INTO TABLE @DATA(lt_user).

      IF lt_user[] IS INITIAL.
        ev_result = REF #( 'A' ).
      ELSE.

        lv_date = sy-datum.
        LOOP AT lt_user ASSIGNING FIELD-SYMBOL(<ls_user>).

          "Validity date check
*          IF <ls_user>-valid_fr = <ls_user>-valid_to.
*            DATA(lv_dat) = <ls_user>-valid_fr.
*            IF lv_date >= lv_dat.
*              <ls_user>-flag = abap_true.
*            ELSE.
*              <ls_user>-flag = abap_false.
*            ENDIF.
*          ELSE.
            IF ( lv_date >= <ls_user>-valid_fr AND
                 lv_date <= <ls_user>-valid_to ).
              <ls_user>-flag = abap_true.
            ELSE.
              <ls_user>-flag = abap_false.
            ENDIF.
*          ENDIF.

        ENDLOOP.

        DELETE lt_user WHERE flag = abap_false.
        IF lt_user[] IS NOT INITIAL.
          ev_result = REF #( 'Y' ).
        ELSE.
          ev_result = REF #( 'N' ).
        ENDIF.

      ENDIF.


*      IF sy-subrc = 0.
*        ev_result = REF #( 'Y' ).
*      ELSE.
*        ev_result = REF #( 'N' ).
*      ENDIF.
    ELSE.
      ev_result = REF #( 'Z' ).
    ENDIF.
  ENDMETHOD.


  METHOD if_fin_re_custom_function~get_description.
    " TODO: Return a message ID/No. for the text description of your function that is to be displayed in the Manage Substitution/Validation Rules app.
    " ABAP: Create the new message class as 'ZMPROJ'
    rs_msg = VALUE symsg( msgid = 'ZFIN' msgno = '003' ). "msgv1 = 'Function to get the Auth. User'
  ENDMETHOD.


  METHOD if_fin_re_custom_function~get_name.
    " Provide a unique name in your customer name space.
    rv_name = c_function_name.
  ENDMETHOD.


  METHOD if_fin_re_custom_function~get_parameters.
    " Create a list of parameters that the function is using.
    " Function parameters contain:
    "   - A unique name,
    "   - A technical ABAP type that can be specified as shown below.
    "     If no type is specified, ABAP string is used as a default.
    "   - A parameter can be flagged as optional. By default, every parameter is mandatory.
    "   - A CDS view can be specified providing a list of values displayed in a value help dialog.
    " A function must have at least one parameter.
    " TODO: Change the return type of your function according to your requirements.
    " ABAP: Specify the field name that use for importing parameter
    " User that created the journal entry Field (FIS_USNAM) is Released but it cannot use to ref.
    " Create the only new data element as 'ZUNAME' (CHAR, 12) and no need to create the domain
    "rt_parameters = VALUE #( ( name = c_parameter_name abap_type = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_name( 'DATS' ) ) ) ).
    rt_parameters = VALUE #( ( name = c_parameter_name abap_type = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_name( 'ZDUNAME' ) ) ) ).
  ENDMETHOD.


  METHOD if_fin_re_custom_function~get_returntype.
    " Specify the ABAP type of the value that is set by the method if_fin_re_custom_function~execute.
    " TODO: Change the return type of your function according to your requirements.
    ro_type =  CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_name( 'ZCL_FIN_PROJECT_AUTH_USER=>T_GET' ) ).
  ENDMETHOD.


  METHOD if_fin_re_custom_function~is_disabled.
    " If you want the function to be visible in all events of the Manage Substitution/Validation Rules app,
    " remove this method completely. If the function should only be visible in specific events,
    " the importing variable iv_event_id can be used and is displayed below.

    " TODO: Set rv_disabled to ABAP_FALSE, depending on your requirements
    " ABAP: Comment the below code
*    IF iv_event_id = 'FINS_ACC_MS_1'.
*      " If set to false, the function will be visible in the business context Market Segment of the Manage Substitution/Validation Rules app, only.
*      rv_disable = abap_true.
*    ELSE.
*      rv_disable = abap_true.
*    ENDIF.
  ENDMETHOD.
ENDCLASS.
