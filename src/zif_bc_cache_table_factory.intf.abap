interface ZIF_BC_CACHE_TABLE_FACTORY
  public .

  METHODS:

    create_s_cache
      RETURNING
        VALUE(rv_result) TYPE REF TO data,

    CREATE_S_CACHE_KEY_DATA
      RETURNING
        VALUE(rv_result) TYPE REF TO data,

    CREATE_S_CACHE_KEY_SEQ
      RETURNING
        VALUE(rv_result) TYPE REF TO data,

    CREATE_S_CACHE_KEY
      RETURNING
        VALUE(rv_result) TYPE REF TO data,

    create_sorted_data_cache
      RETURNING
        VALUE(rv_result) TYPE REF TO data,

    create_cache_aggr_fields
      RETURNING
        VALUE(rv_result) TYPE REF TO data,

    create_seq_iterator
      IMPORTING
        iv_ref_s_cache_seq TYPE REF TO data
      RETURNING
        VALUE(rv_result) TYPE REF TO ZIF_BC_CACHE_SEQ_ITERATOR,

    " Cria uma tabela ordenada pela chave de agregação do cache
    " tirando os campos do tipo sequenciais a través dos quais
    " existe uma agregação de segundo nivel (por exemplo o periodo
    " se acumulamos os valores agregados por periodo).
    create_sorted_data_cache_seq
      RETURNING
        VALUE(rv_result) TYPE REF TO data,

    " Cria uma tabela ordenada pela chave de agregação do cache.
    create_sorted_cache_table
      RETURNING
        VALUE(rv_result) TYPE REF TO data.

endinterface.
