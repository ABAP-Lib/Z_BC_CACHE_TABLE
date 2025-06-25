class ZCL_BC_CACHE_TAB_ORIG_OPER definition
  public
  abstract
  create public .

  public section.

    methods:
      constructor
        IMPORTING
          iv_cache TYPE REF TO ZCL_BC_CACHE_TABLE_INC_AGGR
          iv_original_selection TYPE REF TO ZIF_BC_CACHE_ORIGINAL_SEL
          iv_delete_operation TYPE abap_bool,

      add_record
        IMPORTING
          is_original TYPE any,

      execute
        IMPORTING
          " Se utilizado, serão desconsiderados os os registros acrescentados com add_record
          it_original TYPE ANY TABLE optional
        RAISING
          ZCX_BC_CACHE_TABLE_ERROR.

  protected section.

    DATA:
          lv_original_table_name TYPE string,
          lv_cache TYPE REF TO ZCL_BC_CACHE_TABLE_INC_AGGR,
          lv_ref_t_original TYPE REF TO data,
          lv_original_selection TYPE REF TO ZIF_BC_CACHE_ORIGINAL_SEL,
          lv_delete_operation TYPE abap_bool.

    METHODS:

      insert_original
        importing
          it_original type table,

      update_original
        importing
          it_original type table,

      delete_original
        importing
          it_original type table.

  private section.

ENDCLASS.



CLASS ZCL_BC_CACHE_TAB_ORIG_OPER IMPLEMENTATION.


  METHOD add_record.

    FIELD-SYMBOLS:
                   <lt_original> TYPE SORTED TABLE.

    ASSIGN LV_REF_T_ORIGINAL->* to <lt_original>.

    DATA:
          lv_ref_s_original TYPE REF TO data.

    READ TABLE <lt_original>
    from IS_ORIGINAL
    REFERENCE INTO lv_ref_s_original.

    if sy-subrc eq 0.

      ASSIGN lv_ref_s_original->* to FIELD-SYMBOL(<ls_original>).
      <ls_original> = IS_ORIGINAL.

    else.

      INSERT IS_ORIGINAL
      INTO TABLE <LT_ORIGINAL>.

    ENDIF.

  ENDMETHOD.


  METHOD CONSTRUCTOR.

    LV_CACHE = IV_CACHE.
    lv_original_table_name = me->LV_CACHE->lv_factory->get_original_table_name( ).
    LV_REF_T_ORIGINAL = me->LV_CACHE->lv_factory->create_t_original_sorted( ).
    lv_original_selection = iv_original_selection.
    lv_delete_operation = iv_delete_operation.
  ENDMETHOD.


  METHOD DELETE_ORIGINAL.

    CHECK IT_ORIGINAL IS NOT INITIAL.

    DELETE (lv_original_table_name) FROM TABLE IT_ORIGINAL.

  ENDMETHOD.


  METHOD EXECUTE.


    FIELD-SYMBOLS:
                   <lt_original> TYPE TABLE.

    if IT_ORIGINAL is SUPPLIED.
      ASSIGN IT_ORIGINAL to <LT_ORIGINAL>.
    else.
      ASSIGN LV_REF_T_ORIGINAL->* to <lt_original>.
    ENDIF.

    CHECK <lt_original> is NOT INITIAL.

    DATA:
          LV_REF_S_ORIGINAL TYPE REF TO data.

    LOOP AT <lt_original> REFERENCE INTO LV_REF_S_ORIGINAL.

      LV_CACHE->LOCK_CACHE_RECORDS( LV_REF_S_ORIGINAL ).

    ENDLOOP.

    DATA(lv_ref_t_orig_from_db_sorted) = lv_original_selection->SELECT_FROM_DB(
      IV_FACTORY = LV_CACHE->LV_FACTORY
      IV_REF_T_ORIGINAL = ref #( <LT_ORIGINAL> )
    ).

    FIELD-SYMBOLS:
                   <lt_orig_from_db_sorted> TYPE SORTED TABLE.

    ASSIGN lv_ref_t_orig_from_db_sorted->* to <lt_orig_from_db_sorted>.

    FIELD-SYMBOLS:
                   <ls_original> TYPE any.

    DATA(lv_ref_t_original_insert) = me->LV_CACHE->lv_factory->create_t_original( ).
    DATA(lv_ref_t_original_update) = me->LV_CACHE->lv_factory->create_t_original( ).
    DATA(lv_ref_t_original_delete) = me->LV_CACHE->lv_factory->create_t_original( ).

    FIELD-SYMBOLS:
                   <lt_original_insert> TYPE STANDARD TABLE,
                   <lt_original_update> TYPE STANDARD TABLE,
                   <lt_original_delete> TYPE STANDARD TABLE.

    ASSIGN lv_ref_t_original_insert->* to <lt_original_insert>.
    ASSIGN lv_ref_t_original_update->* to <lt_original_update>.
    ASSIGN lv_ref_t_original_delete->* to <lt_original_delete>.

    LOOP AT <lt_original> ASSIGNING <ls_original>.

      DATA:
            lv_ref_s_orig_from_db TYPE REF TO data.

      READ TABLE <lt_orig_from_db_sorted>
      from <ls_original>
      REFERENCE INTO lv_ref_s_orig_from_db.

      if sy-subrc eq 0.

        if lv_delete_operation eq abap_true.

          LV_CACHE->ADD_DELETE(
            IV_REF_S_ORIGINAL = ref #( <ls_original> )
          ).

          APPEND <ls_original> TO <lt_original_delete>.

        else.

          LV_CACHE->ADD_UPDATE(
            IV_REF_S_ORIGINAL_OLD = lv_ref_s_orig_from_db
            IV_REF_S_ORIGINAL_NEW = ref #( <LS_ORIGINAL> )
          ).

          APPEND <LS_ORIGINAL> TO <lt_original_update>.

        endif.

      else.

        LV_CACHE->ADD_INSERT(
          IV_REF_S_ORIGINAL = ref #( <LS_ORIGINAL> )
        ).

        APPEND <LS_ORIGINAL> TO <LT_ORIGINAL_INSERT>.

      ENDIF.

    ENDLOOP.

    me->DELETE_ORIGINAL( <LT_ORIGINAL_DELETE> ).

    me->UPDATE_ORIGINAL( <LT_ORIGINAL_UPDATE> ).

    me->INSERT_ORIGINAL( <LT_ORIGINAL_INSERT> ).

    LV_CACHE->UPDATE_CACHE( ).

    LV_CACHE->UNLOCK_CACHE_RECORDS( ).

  ENDMETHOD.


  METHOD INSERT_ORIGINAL.

    CHECK IT_ORIGINAL IS NOT INITIAL.

    INSERT (lv_original_table_name) FROM TABLE IT_ORIGINAL.

  ENDMETHOD.


  METHOD UPDATE_ORIGINAL.

    CHECK IT_ORIGINAL IS NOT INITIAL.

    UPDATE (lv_original_table_name) FROM TABLE IT_ORIGINAL.

  ENDMETHOD.
ENDCLASS.
