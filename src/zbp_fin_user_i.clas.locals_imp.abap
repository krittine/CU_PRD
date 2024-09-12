*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

***********************************************************************
*
*          RAP BO provider (i. e. ABAP behavior pool/ABP)
*                  for a RAP demo scenario
*
* - RAP scenario: "RAP calculator" (managed, draft-enabled RAP BO with
*   late numbering)
* - Data model: Consists of a root entity alone.
*   The BDEF defines the behavior for this entity. The definitions in the
*   BDEF determine which methods must be implemented in the ABAP behavior
*   pool (ABP). Note that the view contains many annotations for the
*   SAP Fiori UI.
*
* ----------------------------- NOTE -----------------------------------
* This simplified example is not a real life scenario and rather
* focuses on the technical side by giving an idea how the communication
* and data exchange between a RAP BO consumer, which is a class
* in this case, and RAP BO provider can work. Additionally, it shows
* how the methods for non-standard RAP BO operations might be
* self-implemented in an ABP. The example is intentionally kept
* short and simple and focuses on specific RAP aspects. For this reason,
* the example might not fully meet the requirements of the RAP BO contract.
*
* You can also use side effects to trigger data
* changes (in terms of this example, the recalculation of the calculation
* result) and other things based on data changes in UI scenarios with
* draft-enabled BOs.
*
* The code presented in this class is intended only to support the ABAP
* cheat sheets. It is not intended for direct use in a production system
* environment. The code examples in the ABAP cheat sheets are primarily
* intended to provide a better explanation and visualization of the
* syntax and semantics of ABAP statements, not to solve concrete
* programming tasks. For production application programs, you should
* always work out your own solution for each individual case. There is
* no guarantee for the correctness or completeness of the code.
* Furthermore, there is no legal responsibility or liability for any
* errors or their consequences that may occur when using the the example
* code.
*
***********************************************************************
*             Local handler class lhc_maintain
*
* Contains handler method definitions and implementations as defined
* in the CDS behavior definition (BDEF).
*
***********************************************************************
"! <p class="shorttext synchronized">Local handler class lhc_maintain</p>
"! The class represents a RAP BO provider (i. e. an ABAP behavior pool/ABP) for a RAP demo scenario
"! (managed, draft-enabled RAP BO with late numbering).
CLASS lhc_maintain DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR maintain RESULT result.

    METHODS validate FOR VALIDATE ON SAVE
      IMPORTING keys FOR maintain~validate.

ENDCLASS.


CLASS lhc_maintain IMPLEMENTATION.

  METHOD get_instance_authorizations.

    "Retrieving instances based on requested keys
    READ ENTITIES OF zfin_user_i IN LOCAL MODE
    ENTITY maintain
      FIELDS ( authuser ) ""FIELDS ( fieldname ) | ALL FIELDS
      WITH CORRESPONDING #( keys )
      RESULT DATA(lt_maintain)
      FAILED DATA(ls_maintain_failed).

*    "If the read result is initial, stop further method execution.
*    CHECK status IS NOT INITIAL.
*
*    LOOP AT status ASSIGNING FIELD-SYMBOL(<auth>).
*
*      "If a specific field has a certain value, the deletion should be disallowed.
*      IF requested_authorizations-%delete = if_abap_behv=>mk-on.
*        APPEND VALUE #( %tky = <auth>-%tky
*                        %op  = VALUE #( %delete = COND #( WHEN <auth>-field1 = 'X'
*                                                          THEN if_abap_behv=>auth-unauthorized
*                                                          ELSE if_abap_behv=>auth-allowed ) )
*                      ) TO result.
*
*      ENDIF.
*    ENDLOOP.

  ENDMETHOD.

  METHOD validate.

*   "reading entities from CDS view
    READ ENTITIES OF zfin_user_i IN LOCAL MODE
      ENTITY maintain
      ALL FIELDS ""FIELDS ( fieldname ) | ALL FIELDS
      WITH CORRESPONDING #( keys )
      RESULT DATA(lt_validate)
      FAILED DATA(ls_validate_failed).

*   "if the above read fails then return the error message
    IF ls_validate_failed IS NOT INITIAL.
      failed = CORRESPONDING #( DEEP ls_validate_failed ).
      RETURN.
    ENDIF.

*    AUTHORITY-CHECK OBJECT 'ZOFIN_USER'
*      ID 'ACTVT'      FIELD '03'
*      ID 'ZFFIN_USER' DUMMY.
*    IF sy-subrc <> 0.
*      RETURN.
*    ENDIF.

    LOOP AT lt_validate ASSIGNING FIELD-SYMBOL(<validate>).
      IF <validate>-authuser+0(2) <> 'CB'.
        DATA(lv_msg) = |User Name must begin with 'CB'!|.

*        APPEND VALUE #( %tky              = <validate>-%tky
*                        %state_area       = 'Validate Auth. User'
*                        %msg              = new_message_with_text( severity = if_abap_behv_message=>severity-error
*                                                                   text     = lv_msg )
*                        %element-authuser = if_abap_behv=>mk-on ) TO reported-maintain.

        lv_msg       = COND #( WHEN <validate>-excelrownumber IS INITIAL
                                 THEN lv_msg
                                 ELSE |Row { <validate>-excelrownumber } : { lv_msg }| ).

        APPEND VALUE #( %tky              = <validate>-%tky )     TO failed-maintain.

        APPEND VALUE #( %tky              = <validate>-%tky
                        %state_area       = 'Validate Auth. User'
                        %msg              = new_message_with_text( severity = if_abap_behv_message=>severity-error
                                                                   text     = lv_msg )
                        %element-authuser = if_abap_behv=>mk-on ) TO reported-maintain.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
