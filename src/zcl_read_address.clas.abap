CLASS zcl_read_address DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_READ_ADDRESS IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    cl_address_format=>get_instance( )->printform_postal_addr(

      EXPORTING
*        iv_address_type              = '1'
        iv_address_number            = ''
*        iv_person_number             =
        iv_language_of_country_field = 'E'
*        iv_number_of_lines           = 99
*        iv_sender_country            = space
      IMPORTING
        ev_formatted_to_one_line     = DATA(one_line)
        et_formatted_all_lines       = DATA(all_lines)
    ).

    if all_lines[] is not INITIAL.
      out->write( data = all_lines name = 'Address' ).
    endif.
  ENDMETHOD.
ENDCLASS.
