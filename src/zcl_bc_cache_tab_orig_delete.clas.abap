class ZCL_BC_CACHE_TAB_ORIG_DELETE definition
  public
  inheriting from ZCL_BC_CACHE_TAB_ORIG_OPER
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IV_CACHE type ref to ZCL_BC_CACHE_TABLE_INC_AGGR
      !IV_ORIGINAL_SELECTION type ref to ZIF_BC_CACHE_ORIGINAL_SEL .
  protected section.
  private section.

ENDCLASS.



CLASS ZCL_BC_CACHE_TAB_ORIG_DELETE IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    super->CONSTRUCTOR(
      IV_CACHE              = IV_CACHE
      IV_ORIGINAL_SELECTION = IV_ORIGINAL_SELECTION
      IV_DELETE_OPERATION   = ABAP_TRUE
    ).

  ENDMETHOD.
ENDCLASS.
