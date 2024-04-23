class ZCL_MDGBP_UTILITY definition
  public
  final
  create private.

public section.
  class-methods M_READ_SRT_MONI
  importing
    !IV_BP type BU_PARTNER
    !IV_BP_REL type FLAG default 'X'
  exporting
    !EV_BP_FOUND type FLAG .
protected section.
private section.
ENDCLASS.

CLASS ZCL_MDGBP_UTILITY IMPLEMENTATION.
* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_MDGBP_UTILITY=>M_READ_SRT_MONI
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_BP                          TYPE        BU_PARTNER
* | [--->] IV_BP_REL                      TYPE        FLAG (default ='X')
* | [<---] EV_BP_FOUND                    TYPE        FLAG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD m_read_srt_moni.
* This method is to read and parse the soa messages

    DATA: lo_conv          TYPE REF TO cl_abap_conv_in_ce,
          ld_text          TYPE string,
          lt_property_list TYPE srt_persistency_property_t,
          ls_property_list LIKE LINE OF lt_property_list,
          ld_not_found     TYPE xsdboolean,
          ld_error_text    TYPE string,
          lv_out_bp        TYPE bu_partner.

    DATA: lo_xml      TYPE REF TO cl_xml_document,
          lv_retcode  TYPE sy-subrc,
          lv_node     TYPE REF TO if_ixml_node,
          lv_iterator TYPE REF TO if_ixml_node_iterator,
          lv_name     TYPE string,
          lv_value    TYPE string.

    CONSTANTS: lc_relationship_ws_out TYPE string VALUE 'BusinessPartnerRelationshipSUITEBulkReplicateRequest_Out',
               lc_version             TYPE srt_persistency_version VALUE '001'.

    IF iv_bp_rel EQ abap_true. " In case we want to read another message later we can reuse the same method

      DATA(lv_uzeit) = sy-uzeit - 3600. " Messages from last hour only
      CONVERT DATE sy-datum TIME lv_uzeit
        INTO TIME STAMP DATA(lv_timestamp) TIME ZONE sy-zonlo.

* 1 - We get the messages
      SELECT * FROM srt_mmaster
        WHERE ob_if_name = @lc_relationship_ws_out
        AND created_at >= @lv_timestamp
        INTO TABLE @DATA(lt_srt_mmaster).

* 2 - We parse them
      LOOP AT lt_srt_mmaster INTO DATA(ls_srt_mmaster).
       lo_conv = cl_abap_conv_in_ce=>create( encoding = 'UTF-8' ).

        TRY.
          CALL METHOD cl_soap_db_moni=>get_message_properties
              EXPORTING
                persist_id    = ls_srt_mmaster-persist_id
              IMPORTING
                property_list = lt_property_list
                not_found     = ld_not_found
                error_text    = ld_error_text.
          CATCH cx_wsm_access_authority.
            RETURN.
        ENDTRY.

        IF lt_property_list[] IS NOT INITIAL.
          READ TABLE lt_property_list INDEX 1 INTO ls_property_list.

          lo_conv->convert( EXPORTING input = ls_property_list-protocol_data
                  IMPORTING data  = ld_text ).

          CREATE OBJECT lo_xml.
          CALL METHOD lo_xml->parse_string
            EXPORTING
              stream  = ld_text
            RECEIVING
              retcode = lv_retcode.

          lv_node = lo_xml->find_node( name = 'BusinessPartnerInternalID' ).
          lv_value = lv_node->get_value( ).
          lv_out_bp = lv_value.
          IF lv_out_bp = iv_bp. " We found the BP, lets return
            ev_bp_found = abap_true.
            RETURN.
          ENDIF.

* Here i was iterating the nodes...
*          lv_node = lo_xml->m_document.
*          CHECK NOT lv_node IS INITIAL.
*          lv_iterator = lv_node->create_iterator( ).
*          lv_node = lv_iterator->get_next( ).
*
*          WHILE NOT lv_node IS INITIAL.
*            CASE lv_node->get_type( ).
*
*              WHEN if_ixml_node=>co_node_element.
*                lv_name = lv_node->get_name( ).
*
*              WHEN if_ixml_node=>co_node_text OR
*              if_ixml_node=>co_node_cdata_section.
*                lv_value = lv_node->get_value( ).
*                IF lv_name = 'BusinessPartnerInternalID'.
*                  lv_out_bp = lv_value.
*                  IF lv_out_bp = iv_bp. " We found the BP, lets return
*                    ev_bp_found = abap_true.
*                    RETURN.
*                  ENDIF.
*                ENDIF.
*
*            ENDCASE.
*            lv_node = lv_iterator->get_next( ).
*          ENDWHILE.

        ENDIF.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
