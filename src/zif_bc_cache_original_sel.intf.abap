interface ZIF_BC_CACHE_ORIGINAL_SEL
  public .

  methods:
    select_from_db
      importing
        iv_ref_t_original type REF TO data
        iv_factory TYPE REF TO ZIF_BC_CACHE_TABLE_FACTORY
      returning
        VALUE(rv_ref_t_original_sorted) type REF TO data.

endinterface.
