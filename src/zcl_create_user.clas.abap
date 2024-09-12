CLASS zcl_create_user DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CREATE_USER IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    "DATA: itab  TYPE TABLE OF ztauth_user.
    DATA: itab  TYPE TABLE OF ytest001. "ztfin_user.
    "DATA: itab  TYPE TABLE OF ztabap001.

*  "read current time stamp
    GET TIME STAMP FIELD DATA(ztime).

    DATA: time_stamp TYPE timestamp,
          tz         TYPE tznzone,
          lv_date_fr TYPE datn VALUE '20240618',
          lv_date_to TYPE datn VALUE '20240618',
          lv_date    TYPE datn.

*    tz = 'UTC'.
*    time_stamp = CONV #( '20240617230000.0000000' ).
*    CONVERT TIME STAMP time_stamp TIME ZONE tz
*            INTO DATE DATA(dat_fr) TIME DATA(tim_fr)
*            DAYLIGHT SAVING TIME DATA(dst_fr).
*    out->write( |{ dat_fr DATE = ISO } { tim_fr TIME = ISO } { dst_fr }| ).
*    out->write( |\n| ).
*
*    tz = 'UTC'.
*    time_stamp = CONV #( '20240618110000.0000000' ).
*    CONVERT TIME STAMP time_stamp TIME ZONE tz
*            INTO DATE DATA(dat_to) TIME DATA(tim_to)
*            DAYLIGHT SAVING TIME DATA(dst_to).
*    out->write( |{ dat_to DATE = ISO } { tim_to TIME = ISO } { dst_to }| ).
*    out->write( |\n| ).

*    "Valid From: 17 Jun 2024 9:59:19AM
*    "Valid To: 18 Jun 2024 11:59:29AM

*    "Validity date check
*    "lv_date = '20241231'.
*    lv_date = '20240617'.
*    IF lv_date_fr = lv_date_to.
*      out->write( '1' ).
*      out->write( |\n| ).
*      DATA(lv_dat) = lv_date_fr.
*      IF lv_date  >= lv_dat.
*        out->write( '2' ).
*        out->write( |\n| ).
*      ELSE.
*        out->write( '3' ).
*        out->write( |\n| ).
*      ENDIF.
*    ELSE.
*      IF ( lv_date  >= lv_date_fr AND
*           lv_date  <= lv_date_to ).
*        out->write( 'Valid Date' ).
*        out->write( |\n| ).
*      ELSE.
*        out->write( 'Not Valid Date' ).
*        out->write( |\n| ).
*      ENDIF.
*    ENDIF.

   select * from i_product where product = '*' into TABLE @data(lt_data).

   out->write( lt_data ).

**  "insert the table entries
*    "DELETE FROM ztauth_user.
    DELETE FROM ytest001 WHERE authuser LIKE 'CB%'.
*    DELETE FROM ztabap001.
    out->write( |\n| ).
    out->write( 'Delete successfully!' ). "20240618110000.0000000

*  "fill internal Auth. User into internal table
    itab = VALUE #(

      ( authuser = 'CB9980000002' name = 'ABC')

*    ( authuser = 'CB9980000002' valid_fr = '20240625' valid_to = '20240625'
*      name = 'Theraphat Navanitiwongsakul' created_by = 'CB9980000013' created_at = ztime last_changed_by = 'CB9980000013' last_changed_at = ztime )
*    ( authuser = 'CB9980000002' valid_fr = '20240621' valid_to = '20240621'
*      name = 'Theraphat Navanitiwongsakul' created_by = 'CB9980000013' created_at = ztime last_changed_by = 'CB9980000013' last_changed_at = ztime )
*    ( authuser = 'CB9980000013' valid_fr = '20240621' valid_to = '20240621'
*      name = 'Kanittha Seesura'            created_by = 'CB9980000013' created_at = ztime last_changed_by = 'CB9980000013' last_changed_at = ztime )
*    ( authuser = 'CB9980000002' valid_fr = '20240624' valid_to = '20240624'
*      name = 'Theraphat Navanitiwongsakul' created_by = 'CB9980000013' created_at = ztime last_changed_by = 'CB9980000013' last_changed_at = ztime )

*    ( authuser = 'CB9980000999' valid_fr = '20240620' valid_to = '20240621' name = 'Somchai JaiKla'              created_by = 'CB9980000013' created_at = ztime last_changed_by = 'CB9980000013' last_changed_at = ztime )
*    ( authuser = 'CB9980001000' valid_fr = '20240620' valid_to = '20240621' name = 'Somying JaiDee'              created_by = 'CB9980000013' created_at = ztime last_changed_by = 'CB9980000013' last_changed_at = ztime )
**    ( authuser = 'CB9980000008' valid_fr = '20240620' valid_to = '20240621' name = 'Theraphat Navanitiwongsakul' created_by = 'CB9980000013' created_at = ztime last_changed_by = 'CB9980000013' last_changed_at = ztime )
*    ( authuser = 'CB9980000010' valid_fr = '20240620' valid_to = '20240621' name = 'Sasithorn Katesinchai'       created_by = 'CB9980000013' created_at = ztime last_changed_by = 'CB9980000013' last_changed_at = ztime )
*    ( authuser = 'CB9980000011' valid_fr = '20240620' valid_to = '20240621' name = 'Sicharinee Chaochavanil'     created_by = 'CB9980000013' created_at = ztime last_changed_by = 'CB9980000013' last_changed_at = ztime )
*    ( authuser = 'CB9980000012' valid_fr = '20240620' valid_to = '20240621' name = 'Wongsakorn Swangsringarm'    created_by = 'CB9980000013' created_at = ztime last_changed_by = 'CB9980000013' last_changed_at = ztime )
*    ( authuser = 'CB9980000013' valid_fr = '20240620' valid_to = '20240621' name = 'Krittin Angchunt'            created_by = 'CB9980000013' created_at = ztime last_changed_by = 'CB9980000013' last_changed_at = ztime )
*    ( authuser = 'CB9980000043' valid_fr = '20240620' valid_to = '20240621' name = 'Paranya Natham'              created_by = 'CB9980000013' created_at = ztime last_changed_by = 'CB9980000013' last_changed_at = ztime )
*    ( authuser = 'CB9980000100' valid_fr = '20240620' valid_to = '20240621' name = 'Hemmarat Sukthavorn'         created_by = 'CB9980000013' created_at = ztime last_changed_by = 'CB9980000013' last_changed_at = ztime )
*    ( authuser = 'CB9980000101' valid_fr = '20240620' valid_to = '20240621' name = 'Pornnapat Naprasert'         created_by = 'CB9980000013' created_at = ztime last_changed_by = 'CB9980000013' last_changed_at = ztime )
    ) .

*  "insert the table entries
*    "MODIFY ztauth_user FROM TABLE @itab.
    MODIFY ytest001 FROM TABLE @itab.
*    MODIFY ztabap001 FROM TABLE @itab.
    out->write( data = itab name = 'Internal Table: itab' ).
    out->write( |\n| ).
    out->write( 'Created User successfully!' ).
    out->write( |\n| ).

*    SELECT FROM ztabap001 WITH PRIVILEGED ACCESS
*      FIELDS
*      authuser,
*      valid_fr,
*      valid_to,
*      name,
*      ' '        AS flag
*      WHERE authuser = 'CB9980000999'
*      INTO TABLE @DATA(lt_user).
*    IF sy-subrc = 0.
*      out->write( data = lt_user name = 'Test' ).
*      out->write( |\n| ).
*    ENDIF.

*
*    SELECT SINGLE * FROM i_enterpriseproject
*      WHERE project = 'R.11224004'
*      INTO @DATA(ls_proj).
*    IF sy-subrc = 0.
*      out->write( |\n| ).
*      out->write( ls_proj-project ).
*      out->write( ls_proj-processingstatus ).
*    ENDIF.
*
**   Possible alternatives to the system fields used below
*    DATA(sys_date) = cl_abap_context_info=>get_system_date( ).
*    DATA(sys_time) = cl_abap_context_info=>get_system_time( ).
*    DATA(sys_user) = cl_abap_context_info=>get_user_technical_name( ).
*    IF sy-subrc = 0.
*      out->write( |\n| ).
*      out->write( sys_date ).
*      out->write( sys_time ).
*      out->write( sys_user ).
*    ENDIF.

  ENDMETHOD.
ENDCLASS.
