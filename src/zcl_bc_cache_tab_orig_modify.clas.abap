class ZCL_BC_CACHE_TAB_ORIG_MODIFY definition
  public
  inheriting from ZCL_BC_CACHE_TAB_ORIG_OPER
  create public .

  public section.

    METHODS:

      constructor
        IMPORTING
          iv_cache TYPE REF TO ZCL_BC_CACHE_TABLE_INC_AGGR
          iv_original_selection TYPE REF TO ZIF_BC_CACHE_ORIGINAL_SEL.

  protected section.
  private section.

ENDCLASS.



CLASS ZCL_BC_CACHE_TAB_ORIG_MODIFY IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    super->CONSTRUCTOR(
      IV_CACHE              = IV_CACHE
      IV_ORIGINAL_SELECTION = IV_ORIGINAL_SELECTION
      IV_DELETE_OPERATION   = ABAP_FALSE
    ).

  ENDMETHOD.
ENDCLASS.
