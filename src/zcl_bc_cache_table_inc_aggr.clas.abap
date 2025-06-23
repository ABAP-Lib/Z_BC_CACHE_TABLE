class ZCL_BC_CACHE_TABLE_INC_AGGR definition
  public
  abstract
  create public .

  " Agregação de dados de uma tabela 'original' para uma tabela 'cache'
  " acumulando saldos de forma incemental por periodo.

  public section.

    types TY_REF_S_ORIGINAL type ref to DATA . " Registro com a estrutura original

    methods CONSTRUCTOR
      importing
        IV_FACTORY type ref to ZIF_BC_CACHE_TABLE_FACTORY
        iv_persistance TYPE REF TO ZIF_BC_CACHE_PERSISTANCE.

    " @pre LOCK was called before select IV_REF_S_ORIGINAL from DB
    "      to assure that cache associated to IV_REF_S_ORIGINAL
    "      is not modified by other process.
    methods ADD_INSERT
      importing
        IV_REF_S_ORIGINAL type TY_REF_S_ORIGINAL .

    " @pre LOCK was called before select IV_REF_S_ORIGINAL_OLD from DB
    "      to assure that cache associated to IV_REF_S_ORIGINAL_OLD
    "      is not modified by other process.
    methods ADD_UPDATE
      importing
        IV_REF_S_ORIGINAL_OLD type TY_REF_S_ORIGINAL
        IV_REF_S_ORIGINAL_NEW type TY_REF_S_ORIGINAL .

    " @pre LOCK was called before select IV_REF_S_ORIGINAL from DB
    "      to assure that cache associated to IV_REF_S_ORIGINAL
    "      is not modified by other process.
    methods ADD_DELETE
      importing
        IV_REF_S_ORIGINAL type TY_REF_S_ORIGINAL .

    methods UPDATE_CACHE .

    METHODS:

      lock_cache_records ABSTRACT
        importing
          IV_REF_S_ORIGINAL type TY_REF_S_ORIGINAL
          iv_wait TYPE abap_bool DEFAULT abap_true
        RAISING
          ZCX_BC_CACHE_TABLE_ERROR,

      unlock_cache_records ABSTRACT.

    DATA:
          lv_factory TYPE REF TO ZIF_BC_CACHE_TABLE_FACTORY READ-ONLY.

  protected section.

    DATA:
          lv_PERSISTANCE TYPE REF TO ZIF_BC_CACHE_PERSISTANCE.

    TYPES:
      ty_ref_t_original TYPE REF TO data, " Registro com a estrutura original
      ty_ref_s_cache TYPE REF TO data, " Registro com a estrutura do cache.
      ty_ref_t_cache TYPE REF TO data, " Registros com a estrutura do cache ordenado pela chave.
      ty_ref_s_cache_key_data TYPE REF TO data, " Registro com a estrutura do cache só com campos de chave sem seq.
      ty_ref_t_cache_key_data TYPE REF TO data, " Registros ordenados com a estrutura do cache só com campos de chave sem seq.
      ty_ref_s_cache_min_seq_by_data TYPE REF TO data, " Registro com a estrutura do cache pero sem valores agregados.
      ty_ref_t_cache_min_seq_by_data TYPE REF TO data, " Registros com a estrutura do cache pero sem valores agregados ordenados pelo data (unico seq, por data).
      ty_ref_s_cache_key_seq TYPE REF TO data,
      ty_ref_s_cache_aggr_fields TYPE REF TO data,
      ty_ref_s_cache_key TYPE REF TO data.


    METHODS:

      get_ref_s_cache_from_key
        IMPORTING
          iv_ref_t_cache TYPE ty_ref_t_cache
          iv_ref_s_cache_key_data TYPE ty_ref_s_cache_key_data
          iv_ref_s_cache_key_seq TYPE ty_ref_s_cache_key_seq
        RETURNING
          VALUE(rv_result) TYPE ty_ref_s_cache,

      get_ref_t_cach_data_frm_minseq
        IMPORTING
          iv_ref_t_cache_min_seq TYPE ty_ref_t_cache_min_seq_by_data
        RETURNING
          VALUE(rv_result) TYPE ty_ref_t_cache_key_data,

      get_cache_min_seq_by_data
        IMPORTING
          iv_ref_t_cache TYPE ty_ref_t_cache
        RETURNING
          VALUE(rv_result) TYPE ty_ref_t_cache_min_seq_by_data,

      collect_cache_to_cache
        IMPORTING
          iv_ref_t_cache_from TYPE ty_ref_t_cache
          iv_ref_t_cache_to TYPE ty_ref_t_cache,

      get_or_insert_cache_from_orig
        IMPORTING
          iv_ref_s_original TYPE ty_ref_s_original
          iv_ref_t_cache_sorted TYPE ty_ref_t_cache
        RETURNING
          VALUE(rv_result) TYPE ty_ref_s_cache,

      add_operation
        IMPORTING
          iv_ref_t_cache_operation type ty_ref_t_cache
          iv_ref_s_original TYPE ty_ref_s_original
        RETURNING
          VALUE(rv_result) TYPE ty_ref_s_cache,

      get_cache_key_data
        IMPORTING
          iv_ref_s_cache_data_like TYPE data
        RETURNING
          VALUE(rv_result) TYPE ty_ref_s_cache_key_data,

      get_cache_key_seq
        IMPORTING
            iv_ref_s_cache_seq_like TYPE data
        RETURNING
          VALUE(rv_result) TYPE ty_ref_s_cache_key_seq. " cache record


    DATA:
          lv_ref_t_original TYPE ty_ref_t_original,
          lv_ref_t_cache_inserts type ty_ref_t_cache,
          lv_ref_t_cache_updates type ty_ref_t_cache,
          lv_ref_t_cache_deletes type ty_ref_t_cache.

    " Mandatory redefinitions

    METHODS:

      " faz agregação dos valores de iv_ref_s_cache, em iv_ref_s_aggregated_fields.
      aggregate_cache ABSTRACT
        IMPORTING
          iv_ref_s_cache TYPE ty_ref_s_cache
          iv_ref_s_aggregated_fields TYPE ty_ref_s_cache_aggr_fields,

      " Obtem o nome do ultimo campo do conjunto data da chave do cache.
      get_cache_key_data_last_field ABSTRACT
        RETURNING
          VALUE(RV_RESULT) TYPE string,

      " Elimina os registros que ficaram com delta nulo.
      delete_initial_cache ABSTRACT
        IMPORTING
          iv_ref_t_cache TYPE ty_ref_t_cache,

      " Faz agregação de um registro de cache em outro.
      " Deveriam ter a mesma chave.
      aggregate_cache_to_cache ABSTRACT
        IMPORTING
          iv_ref_s_cache_from type ty_ref_s_cache
          iv_ref_s_cache_to type ty_ref_s_cache,

      " Faz agregação de um registro original num registro de cache.
      " Deveriam ter a mesma chave.
      aggregate_original_to_cache ABSTRACT
        IMPORTING
          iv_ref_s_original type ty_ref_s_original
          iv_ref_s_cache type ty_ref_s_cache,

      " Faz a operação inversa da agregação de um registro original
      " num registro de cache.
      " Deveriam ter a mesma chave.
      deaggregate_original_to_cache ABSTRACT
        IMPORTING
          iv_ref_s_original type ty_ref_s_original
          iv_ref_s_cache type ty_ref_s_cache,

      " Obtém as chaves de um registro original, num registro de cache.
      get_cache_keys_from_orig ABSTRACT
        IMPORTING
          iv_ref_s_original TYPE ty_ref_s_original
        RETURNING
          VALUE(rv_result) TYPE ty_ref_s_cache.

  private section.

ENDCLASS.



CLASS ZCL_BC_CACHE_TABLE_INC_AGGR IMPLEMENTATION.


  METHOD add_delete.

    DATA(lv_ref_s_cache) = me->add_operation(
      iv_ref_t_cache_operation = lv_ref_t_cache_deletes
      iv_ref_s_original = iv_ref_s_original
    ).

    me->deaggregate_original_to_cache(
      iv_ref_s_original = iv_ref_s_original
      iv_ref_s_cache = lv_ref_s_cache
    ).

  endmethod.


  METHOD add_insert.

    DATA(lv_ref_s_cache) = me->add_operation(
      iv_ref_t_cache_operation = lv_ref_t_cache_inserts
      iv_ref_s_original = iv_ref_s_original
    ).

    me->aggregate_original_to_cache(
      iv_ref_s_original = iv_ref_s_original
      iv_ref_s_cache = lv_ref_s_cache
    ).

  endmethod.


  METHOD add_operation.

    rv_result = me->get_or_insert_cache_from_orig(
      iv_ref_s_original = iv_ref_s_original
      iv_ref_t_cache_sorted = iv_ref_t_cache_operation
    ).

  endmethod.


  METHOD add_update.

    DATA(lv_ref_s_cache_old) = me->add_operation(
      iv_ref_t_cache_operation = lv_ref_t_cache_updates
      iv_ref_s_original = iv_ref_s_original_old
    ).

    me->deaggregate_original_to_cache(
      iv_ref_s_original = iv_ref_s_original_old
      iv_ref_s_cache = lv_ref_s_cache_old
    ).

    DATA(lv_ref_s_cache_new) = me->add_operation(
      iv_ref_t_cache_operation = lv_ref_t_cache_updates
      iv_ref_s_original = iv_ref_s_original_new
    ).

    me->aggregate_original_to_cache(
      iv_ref_s_original = iv_ref_s_original_new
      iv_ref_s_cache = lv_ref_s_cache_new
    ).

  endmethod.


  METHOD collect_cache_to_cache.

    FIELD-SYMBOLS:
                   <LT_CACHE_FROM> TYPE SORTED TABLE,
                   <lt_cache_to> TYPE SORTED TABLE.

    ASSIGN IV_REF_T_CACHE_FROM->* to <LT_CACHE_FROM>.
    ASSIGN IV_REF_T_CACHE_TO->* to <lt_cache_to>.

    DATA:
          lv_ref_s_CACHE_FROM TYPE TY_REF_S_CACHE.

    LOOP AT <LT_CACHE_FROM> REFERENCE INTO lv_ref_s_CACHE_FROM.

      ASSIGN lv_ref_s_CACHE_FROM->* to FIELD-SYMBOL(<ls_CACHE_FROM>).

      DATA:
            lv_ref_s_cache_to TYPE TY_REF_S_CACHE.

      READ TABLE <lt_cache_to>
      from <ls_CACHE_FROM>
      REFERENCE INTO lv_ref_s_cache_to.

      if sy-subrc ne 0.

        INSERT <ls_CACHE_FROM>
        INTO TABLE <lt_cache_to>.

        CONTINUE.

      ENDIF.

      me->aggregate_cache_to_cache(
        iv_ref_s_cache_from = lv_ref_s_cache_from
        iv_ref_s_cache_to = lv_ref_s_cache_to
      ).

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    lv_factory = iv_factory.

    lv_ref_t_cache_inserts = iv_factory->create_sorted_cache_table( ).
    lv_ref_t_cache_updates = iv_factory->create_sorted_cache_table( ).
    lv_ref_t_cache_deletes = iv_factory->create_sorted_cache_table( ).

    lv_persistance = iv_persistance.

  endmethod.


  METHOD get_cache_key_data.
*    IMPORTING
*      iv_ref_s_cache_data_like TYPE ty_ref_s_cache_key
*    RETURNING
*      VALUE(rv_result) TYPE ty_ref_s_cache_key_data,

    ASSIGN iv_ref_s_cache_data_like->* to FIELD-SYMBOL(<ls_cache_data_like>).
    rv_result = LV_FACTORY->CREATE_S_CACHE_KEY_DATA( ).
    ASSIGN rv_result->* to FIELD-SYMBOL(<LS_CACHE_KEY_DATA>).

    MOVE-CORRESPONDING <ls_cache_data_like> to <LS_CACHE_KEY_DATA>.

  ENDMETHOD.


  METHOD get_cache_key_seq.
*    IMPORTING
*        iv_ref_s_cache_min_seq TYPE ty_ref_s_cache_key
*    RETURNING
*      VALUE(rv_result) TYPE ty_ref_s_cache_key_seq,

    ASSIGN iv_ref_s_cache_seq_like->* to FIELD-SYMBOL(<ls_cache_seq_like>).
    rv_result = LV_FACTORY->CREATE_S_CACHE_KEY_SEQ( ).
    ASSIGN rv_result->* to FIELD-SYMBOL(<LS_CACHE_KEY_seq>).

    MOVE-CORRESPONDING <ls_cache_seq_like> to <LS_CACHE_KEY_seq>.

  ENDMETHOD.


  METHOD get_cache_min_seq_by_data.

    rv_result = lv_factory->create_sorted_data_cache_seq( ).

    FIELD-SYMBOLS:
                   <LT_CACHE> TYPE SORTED TABLE,
                   <LT_CACHE_MIN_SEQ_BY_DATA> TYPE SORTED TABLE.

    data(lv_key_data_last_field) = me->get_cache_key_data_last_field( ).

    ASSIGN IV_REF_T_CACHE->* to <LT_CACHE>.
    ASSIGN rv_result->* to <LT_CACHE_MIN_SEQ_BY_DATA>.

    LOOP AT <LT_CACHE> ASSIGNING FIELD-SYMBOL(<LS_CACHE>).

      AT NEW (LV_KEY_DATA_LAST_FIELD).

        DATA(LV_REF_CACHE_MIN_SEQ_BY_DATA) = LV_FACTORY->CREATE_S_CACHE_KEY( ).

        ASSIGN LV_REF_CACHE_MIN_SEQ_BY_DATA->* to FIELD-SYMBOL(<LS_CACHE_MIN_SEQ_BY_DATA>).

        MOVE-CORRESPONDING <LS_CACHE> TO <LS_CACHE_MIN_SEQ_BY_DATA>.

        INSERT <LS_CACHE_MIN_SEQ_BY_DATA>
        INTO TABLE <LT_CACHE_MIN_SEQ_BY_DATA>.

      ENDAT.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_or_insert_cache_from_orig.

    FIELD-SYMBOLS:
                   <lt_cache_sorted> TYPE SORTED TABLE.

    ASSIGN iv_ref_t_cache_sorted->* to <lt_cache_sorted>.

    DATA(lv_ref_s_cache) = me->get_cache_keys_from_orig(
      iv_ref_s_original
    ).

    ASSIGN lv_ref_s_cache->* to FIELD-SYMBOL(<ls_cache>).

    READ TABLE <lt_cache_sorted>
    from <ls_cache>
    REFERENCE INTO rv_result.

    if sy-subrc eq 0.
      return.
    ENDIF.

    INSERT <ls_cache> INTO TABLE <lt_cache_sorted> REFERENCE INTO rv_result.

  ENDMETHOD.


  METHOD get_ref_s_cache_from_key.
*        IMPORTING
*          iv_ref_t_cache TYPE ty_ref_t_cache
*          iv_ref_s_cache_key_data TYPE ty_ref_s_cache_key_data
*          iv_ref_s_cache_key_seq TYPE ty_ref_s_cache_key_seq
*        RETURNING
*          VALUE(rv_result) TYPE ty_ref_s_cache,

    data(lv_ref_s_cache) = me->LV_FACTORY->create_s_cache( ).
    ASSIGN lv_ref_s_cache->* to FIELD-SYMBOL(<ls_cache>).

    ASSIGN iv_ref_s_cache_key_data->* to FIELD-SYMBOL(<ls_cache_key_data>).
    ASSIGN iv_ref_s_cache_key_seq->* to FIELD-SYMBOL(<ls_cache_key_seq>).

    MOVE-CORRESPONDING <ls_cache_key_data> to <ls_cache>.
    MOVE-CORRESPONDING <ls_cache_key_seq> to <ls_cache>.

    FIELD-SYMBOLS:
                   <lt_cache> TYPE SORTED TABLE.

    ASSIGN iv_ref_t_cache->* to <LT_CACHE>.

    READ TABLE <LT_CACHE>
    from <ls_cache>
    REFERENCE INTO rv_result.

  ENDMETHOD.


  METHOD get_ref_t_cach_data_frm_minseq.

*        IMPORTING
*          iv_ref_t_cache_min_seq TYPE ty_ref_t_cache_min_seq_by_data
*        RETURNING
*          VALUE(rv_result) TYPE lv_ref_t_cache_keys_data,

    rv_result = lv_factory->create_sorted_data_cache( ).

    FIELD-SYMBOLS:
                   <LT_CACHE_DATA> TYPE SORTED TABLE,
                   <lt_cache_min_seq> TYPE SORTED TABLE.

    ASSIGN iv_ref_t_cache_min_seq->* to <lt_cache_min_seq>.
    ASSIGN rv_result->* to <LT_CACHE_DATA>.

    LOOP AT <lt_cache_min_seq> ASSIGNING FIELD-SYMBOL(<ls_cache_min_seq>).

      DATA(LV_REF_S_CACHE_KEY_DATA) = LV_FACTORY->CREATE_S_CACHE_KEY_DATA( ).

      ASSIGN LV_REF_S_CACHE_KEY_DATA->* to FIELD-SYMBOL(<LS_CACHE_KEY_DATA>).

      MOVE-CORRESPONDING <ls_cache_min_seq> TO <LS_CACHE_KEY_DATA>.

      INSERT <LS_CACHE_KEY_DATA>
      INTO TABLE <LT_CACHE_DATA>.

    ENDLOOP.

  endmethod.


  METHOD update_cache.

    DATA(lv_ref_t_cache_deltas) = lv_factory->create_sorted_cache_table( ).

    " Calculamos o delta por periodo para todas as operações realizadas.

    me->collect_cache_to_cache(
      iv_ref_t_cache_from = lv_ref_t_cache_inserts
      iv_ref_t_cache_to = lv_ref_t_cache_deltas
    ).

    me->collect_cache_to_cache(
      iv_ref_t_cache_from = lv_ref_t_cache_updates
      iv_ref_t_cache_to = lv_ref_t_cache_deltas
    ).

    me->collect_cache_to_cache(
      iv_ref_t_cache_from = lv_ref_t_cache_deletes
      iv_ref_t_cache_to = lv_ref_t_cache_deltas
    ).

    " Eliminamos os registros que ficaram com delta nulo.

    me->delete_initial_cache(
      iv_ref_t_cache = lv_ref_t_cache_deltas
    ).

    " Se tivessemos uma chave com campos e periodos,
    " o seq seria o periodo e data seriam os outros
    " campos da chave.
    DATA(lv_ref_t_cache_min_seq) = me->get_cache_min_seq_by_data(
      iv_ref_t_cache = lv_ref_t_cache_deltas
    ).

    data(lv_ref_t_cache_keys_data) = me->get_ref_t_cach_data_frm_minseq(
      iv_ref_t_cache_min_seq = lv_ref_t_cache_min_seq
    ).

    lv_persistance->set_cache_min_seq_by_data(
      lv_ref_t_cache_min_seq
    ).

    lv_persistance->select_current_cache_records( ).

    FIELD-SYMBOLS:
                   <lt_cache_min_seq> TYPE any table.

    ASSIGN lv_ref_t_cache_min_seq->* to <lt_cache_min_seq>.

    DATA:
          lv_ref_s_cache_min_seq TYPE ty_ref_s_cache_min_seq_by_data.

    LOOP AT <lt_cache_min_seq> REFERENCE INTO lv_ref_s_cache_min_seq.

      data(lv_ref_s_cache_data) = me->get_cache_key_data( lv_ref_s_cache_min_seq ).

      " Inicializamos uma estrutura com os campos agregados.
      data(lv_ref_s_aggregated_fields) = LV_FACTORY->create_cache_aggr_fields( ). "- delta_v_acum = 0

      data(lv_ref_s_cache_seq) = get_cache_key_seq( lv_ref_s_cache_min_seq ).

      DATA(lv_seq_iterator) = LV_FACTORY->create_seq_iterator( lv_ref_s_cache_seq ).

      " Por cada periodo desde lt_delta_periodo_min[chave].periodo hasta hoje.periodo:
      DO.

        DATA(lv_ref_s_cache_seq_current) = lv_seq_iterator->next( ).

        if lv_ref_s_cache_seq_current is INITIAL.
          EXIT.
        ENDIF.

*       - obter r para lt_delta_periodo[chave,periodo]
        DATA(lv_ref_s_cache_delta) = get_ref_s_cache_from_key(
          iv_ref_t_cache = lv_ref_t_cache_deltas
          iv_ref_s_cache_key_data = lv_ref_s_cache_data
          iv_ref_s_cache_key_seq = lv_ref_s_cache_seq_current
        ).

*       - Se r existe: " Se hay delta para el periodo iterado, incrementamos el delta con el valor indicado.
        if lv_ref_s_cache_delta is BOUND.

*         - delta_v_acum += r.v
          me->aggregate_cache(
            iv_ref_s_cache = lv_ref_s_cache_delta
            iv_ref_s_aggregated_fields = lv_ref_s_aggregated_fields
          ).

        ENDIF.

*       - saldo_3_acum = lt_saldo_3_acum_atual[chave, periodo]
*       - Se saldo_3_acum existe:
*         - saldo_3_acum.v += delta_v_acum
*         - Se não:
*           - saldo_3_acum.chave = lt_delta_periodo_min[chave].chave
*           - insert saldo_3_acum into lt_saldo_3_acum_atual
        lv_persistance->aggr_or_create_cache_record(
          iv_ref_s_cache_key_data = lv_ref_s_cache_data
          iv_ref_s_cache_key_seq = lv_ref_s_cache_seq_current
          iv_ref_s_aggregated_fields = lv_ref_s_aggregated_fields
        ).


      ENDDO.

    ENDLOOP.

*" Atualização do DB
*modify saldo_3_acum from lt_saldo_3_acum_atual
*
    lv_persistance->save( ).


*"(*) O controle transacional pode ser feito a nivel de chave. OK
*"(*) Deve ser realizado o respetivo bloqueio a nivel de chave. OK

  ENDMETHOD.
ENDCLASS.
