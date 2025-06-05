interface ZIF_BC_CACHE_PERSISTANCE
  public .

  METHODS:

    save,

    " locks the keys data defined by set_cache_min_seq_by_data
    lock,

    " unlock what was locked by lock.
    unlock,

    " Agregate the record defined by iv_ref_s_cache_key_data and
    " iv_ref_s_cache_key_seq, reading the records retrieved by
    " select_current_cache_records and using the values in iv_ref_s_aggregated_fields
    " If the record not exists must be created.
    aggr_or_create_cache_record
      IMPORTING
        iv_ref_s_cache_key_data TYPE REF TO data
        iv_ref_s_cache_key_seq TYPE REF TO data
        iv_ref_s_aggregated_fields TYPE REF TO data,

    " selects the records to work with defined by
    " set_cache_min_seq_by_data (from min seq to current seq).
    select_current_cache_records,

    " Define which keys data are going to be used to persist,
    " and from which seq.
    set_cache_min_seq_by_data
      IMPORTING
        iv_ref_t_cache_min_seq_by_data TYPE REF TO data.

endinterface.
