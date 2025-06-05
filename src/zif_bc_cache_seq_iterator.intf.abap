interface ZIF_BC_CACHE_SEQ_ITERATOR
  public .

  METHODS:

    " Retorna o seq. atual, e avança o iterador.
    next
      RETURNING
        VALUE(rv_result) TYPE REF TO data.

endinterface.
