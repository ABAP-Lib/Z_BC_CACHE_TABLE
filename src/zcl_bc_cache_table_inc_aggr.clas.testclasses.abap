*"* use this source file for your ABAP unit test classes

class lcl_types DEFINITION.

  PUBLIC SECTION.

    TYPES:

      BEGIN OF ty_S_CACHE_KEY_DATA,
        data1(30) TYPE c,
        data2(30) TYPE c,
      END OF ty_S_CACHE_KEY_DATA,

      BEGIN OF ty_S_CACHE_KEY_SEQ,
        ano(4) TYPE n,
        mes(2) TYPE n,
      END OF ty_S_CACHE_KEY_SEQ,

      BEGIN OF ty_S_CACHE_KEY.
        INCLUDE TYPE ty_S_CACHE_KEY_DATA as data.
        INCLUDE TYPE ty_S_CACHE_KEY_SEQ as seq.
    TYPES:
      END OF TY_S_CACHE_KEY,

      BEGIN OF ty_s_cache_aggr_fields,
        valor1 TYPE i,
      END OF TY_S_CACHE_AGGR_FIELDS,

      BEGIN OF ty_s_cache.
        INCLUDE TYPE ty_S_CACHE_KEY as key.
        INCLUDE TYPE ty_s_cache_aggr_fields as aggr.
    TYPES:
      END OF TY_S_CACHE,

      ty_t_sorted_cache_data TYPE SORTED TABLE OF ty_S_CACHE_KEY_DATA WITH UNIQUE KEY TABLE_LINE,
      ty_t_sorted_cache_seq_by_data TYPE SORTED TABLE OF ty_S_CACHE_KEY WITH UNIQUE KEY data,
      ty_t_sorted_cache TYPE SORTED TABLE OF ty_S_CACHE WITH UNIQUE KEY key.

ENDCLASS.

CLASS lcl_seq_iterator DEFINITION.

  PUBLIC SECTION.

    INTERFACES:

      ZIF_BC_CACHE_SEQ_ITERATOR.

    METHODS:
      constructor
        IMPORTING
          is_cache_seq_from TYPE lcl_types=>ty_S_CACHE_KEY_SEQ
          iS_CACHE_seq_to TYPE lcl_types=>ty_S_CACHE_KEY_SEQ OPTIONAL.

  PROTECTED SECTION.

    DATA:
          ls_cache_seq_from TYPE lcl_types=>ty_S_CACHE_KEY_SEQ,
          lS_CACHE_seq_current TYPE lcl_types=>ty_S_CACHE_KEY_SEQ,
          lS_CACHE_seq_to TYPE lcl_types=>ty_S_CACHE_KEY_SEQ.

endclass.

CLASS lcl_seq_iterator IMPLEMENTATION.

  METHOD CONSTRUCTOR.

    ls_cache_seq_from = is_cache_seq_from.
    lS_CACHE_seq_current = is_cache_seq_from.

    if iS_CACHE_seq_to is INITIAL.
      lS_CACHE_seq_to-ano = sy-datum(4).
      lS_CACHE_seq_to-mes = sy-datum+4(2).
    else.
      lS_CACHE_seq_to = iS_CACHE_seq_to.
    ENDIF.

  ENDMETHOD.

  METHOD ZIF_BC_CACHE_SEQ_ITERATOR~NEXT.

    if lS_CACHE_seq_current > lS_CACHE_seq_to.
      RETURN.
    ENDIF.

    CREATE DATA RV_RESULT TYPE lcl_types=>ty_S_CACHE_KEY_SEQ.

    DATA(lv_ref_s_CACHE_KEY_SEQ) = cast lcl_types=>ty_S_CACHE_KEY_SEQ( RV_RESULT ).

    lv_ref_s_CACHE_KEY_SEQ->* = lS_CACHE_seq_current.

    data(lv_date) = conv d( |{ lS_CACHE_seq_current-ano }{ lS_CACHE_seq_current-mes }01| ).
    lv_date = lv_date + 32.

    lS_CACHE_seq_current-ano = lv_date(4).
    lS_CACHE_seq_current-mes = lv_date+4(2).

  ENDMETHOD.

endclass.

CLASS lcl_persistance DEFINITION.

  PUBLIC SECTION.

    INTERFACES:
      ZIF_BC_CACHE_PERSISTANCE.

    METHODS:
      constructor
        IMPORTING
          it_cache_initial_data TYPE LCL_TYPES=>TY_T_SORTED_CACHE.

    DATA:
          lt_cache_db_table TYPE LCL_TYPES=>TY_T_SORTED_CACHE READ-ONLY,
          lt_cache_records_to_update TYPE LCL_TYPES=>TY_T_SORTED_CACHE READ-ONLY.

  PROTECTED SECTION.

    DATA:
          lv_REF_T_CACHE_MIN_SEQ_BY_DATA TYPE REF TO LCL_TYPES=>TY_T_SORTED_CACHE_SEQ_BY_DATA.

ENDCLASS.

CLASS LCL_PERSISTANCE IMPLEMENTATION.

    METHOD constructor.

      LT_CACHE_DB_TABLE = IT_CACHE_INITIAL_DATA.

    ENDMETHOD.

    METHOD ZIF_BC_CACHE_PERSISTANCE~save.

      LOOP AT me->LT_CACHE_RECORDS_TO_UPDATE ASSIGNING FIELD-SYMBOL(<LS_CACHE_RECORDS_TO_UPDATE>).

        READ TABLE LT_CACHE_DB_TABLE
        from <LS_CACHE_RECORDS_TO_UPDATE>
        ASSIGNING FIELD-SYMBOL(<LS_CACHE_DB_TABLE>).

        if sy-SUBRC eq 0.

          <LS_CACHE_DB_TABLE>-AGGR = <LS_CACHE_RECORDS_TO_UPDATE>-AGGR.

        else.

          INSERT <LS_CACHE_RECORDS_TO_UPDATE> INTO TABLE LT_CACHE_DB_TABLE.

        ENDIF.

      ENDLOOP.

    endmethod.

    METHOD ZIF_BC_CACHE_PERSISTANCE~lock.
    endmethod.

    METHOD ZIF_BC_CACHE_PERSISTANCE~unlock.
    endmethod.

    METHOD ZIF_BC_CACHE_PERSISTANCE~aggr_or_create_cache_recor.

*        iv_ref_s_cache_key_data TYPE REF TO data
*        iv_ref_s_cache_key_seq TYPE REF TO data
*        iv_ref_s_aggregated_fields TYPE REF TO data,
      DATA:
            ls_cache TYPE LCL_TYPES=>TY_S_CACHE.

      ls_cache-KEY-DATA = CAST LCL_TYPES=>TY_S_CACHE_KEY_DATA(
        iv_ref_s_cache_key_data
      )->*.

      ls_cache-KEY-SEQ = CAST LCL_TYPES=>TY_S_CACHE_KEY_SEQ(
        iv_ref_s_cache_key_seq
      )->*.

      READ TABLE lt_cache_records_to_update
      from ls_cache
      ASSIGNING FIELD-SYMBOL(<ls_cache_records_to_update>).

      if sy-subrc ne 0.

        INSERT ls_cache into TABLE lt_cache_records_to_update
        ASSIGNING <ls_cache_records_to_update>.

      ENDIF.

      <ls_cache_records_to_update>-AGGR-VALOR1 =
        <ls_cache_records_to_update>-AGGR-VALOR1
        + cast ty_s_aggregated_fields(
          iv_ref_s_aggregated_fields )->valor1.

    endmethod.

    METHOD ZIF_BC_CACHE_PERSISTANCE~select_current_cache_records.

      FIELD-SYMBOLS:
        <LS_CACHE_MIN_SEQ_BY_DATA> TYPE LCL_TYPES=>TY_S_CACHE_KEY.

      LOOP at lv_REF_T_CACHE_MIN_SEQ_BY_DATA->* ASSIGNING <LS_CACHE_MIN_SEQ_BY_DATA>.

        READ TABLE me->LT_CACHE_DB_TABLE
        from CORRESPONDING #( <LS_CACHE_MIN_SEQ_BY_DATA> )
        TRANSPORTING NO FIELDS.

        DATA(lv_from) = sy-tabix.

        LOOP AT me->LT_CACHE_DB_TABLE ASSIGNING FIELD-SYMBOL(<LS_CACHE_DB_TABLE>) from lv_from.

          if <LS_CACHE_DB_TABLE>-KEY-DATA ne <LS_CACHE_MIN_SEQ_BY_DATA>-DATA.
            exit.
          ENDIF.

          INSERT <LS_CACHE_DB_TABLE> INTO TABLE me->LT_CACHE_RECORDS_TO_UPDATE.

        ENDLOOP.

      ENDLOOP.


    endmethod.

    METHOD ZIF_BC_CACHE_PERSISTANCE~set_cache_min_seq_by_data.

      data(lv_REF_T_CACHE_MIN_SEQ_BY_DATA) = CAST LCL_TYPES=>TY_T_SORTED_CACHE_SEQ_BY_DATA(
        IV_REF_T_CACHE_MIN_SEQ_BY_DATA ).

    endmethod.

ENDCLASS.

CLASS lcl_factory DEFINITION.

  PUBLIC SECTION.

    INTERFACES:

      ZIF_BC_CACHE_TABLE_FACTORY.

endclass.

CLASS lcl_factory IMPLEMENTATION.

  METHOD ZIF_BC_CACHE_TABLE_FACTORY~CREATE_CACHE_AGGR_FIELDS.

    CREATE DATA RV_RESULT TYPE lcl_types=>TY_S_CACHE_AGGR_FIELDS.

  ENDMETHOD.

  METHOD ZIF_BC_CACHE_TABLE_FACTORY~create_s_cache.

    CREATE DATA RV_RESULT TYPE lcl_types=>TY_S_CACHE.

  ENDMETHOD.

  METHOD ZIF_BC_CACHE_TABLE_FACTORY~create_persistance.

    " @todo

  ENDMETHOD.

  METHOD ZIF_BC_CACHE_TABLE_FACTORY~CREATE_S_CACHE_KEY_DATA.

    CREATE DATA RV_RESULT TYPE lcl_types=>TY_S_CACHE_key_data.

  ENDMETHOD.

  METHOD ZIF_BC_CACHE_TABLE_FACTORY~CREATE_S_CACHE_KEY_SEQ.

    CREATE DATA RV_RESULT TYPE lcl_types=>TY_S_CACHE_key_seq.

  ENDMETHOD.

  METHOD ZIF_BC_CACHE_TABLE_FACTORY~CREATE_S_CACHE_KEY.

    CREATE DATA RV_RESULT TYPE lcl_types=>TY_S_CACHE_key.

  ENDMETHOD.

  METHOD ZIF_BC_CACHE_TABLE_FACTORY~create_sorted_data_cache.

    CREATE DATA RV_RESULT TYPE lcl_types=>TY_T_SORTED_CACHE_DATA.

  ENDMETHOD.

  METHOD ZIF_BC_CACHE_TABLE_FACTORY~create_seq_iterator.

    DATA(lv_ref_s_cache_seq) = cast lcl_types=>TY_S_CACHE_KEY_SEQ( IV_REF_S_CACHE_SEQ ).

    RV_RESULT = NEW LCL_SEQ_ITERATOR(
        IS_CACHE_SEQ_FROM = lv_ref_s_cache_seq->*
*        IS_CACHE_SEQ_TO   =
    ).

  ENDMETHOD.

  METHOD ZIF_BC_CACHE_TABLE_FACTORY~create_sorted_data_cache_seq.

    CREATE DATA RV_RESULT TYPE lcl_types=>TY_T_SORTED_CACHE_SEQ_BY_DATA.

  ENDMETHOD.

  METHOD ZIF_BC_CACHE_TABLE_FACTORY~create_sorted_cache_table.

    CREATE DATA RV_RESULT TYPE lcl_types=>TY_T_SORTED_CACHE.

  ENDMETHOD.

endclass.


CLASS ltcl_test DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.

    METHODS:
      test01 for TESTING.

ENDCLASS.

class LTCL_TEST IMPLEMENTATION.

  METHOD TEST01.

*    lv_cache = new ZCL_BC_CACHE_TABLE_INC_AGGR(
*      IV_FACTORY = new lcl_factory( )
*    ).

    CL_ABAP_UNIT_ASSERT=>FAIL(
*      exporting
*        MSG    =     " Description
*        LEVEL  = IF_AUNIT_CONSTANTS=>CRITICAL    " Severity (TOLERABLE, >CRITICAL<, FATAL)
*        QUIT   = IF_AUNIT_CONSTANTS=>METHOD    " Alter control flow/ quit test (NO, >METHOD<, CLASS)
*        DETAIL =     " Further Description
    ).

  ENDMETHOD.

ENDCLASS.
