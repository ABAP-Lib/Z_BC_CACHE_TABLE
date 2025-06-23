*"* use this source file for your ABAP unit test classes

class lcl_types DEFINITION.

  PUBLIC SECTION.

    TYPES:

      BEGIN OF ty_s_original,
        data0(30) TYPE c,
        data1(30) TYPE c,
        data2(30) TYPE c,
        date TYPE d,
        valor1 TYPE i,
      END OF ty_s_original,

      ty_t_original TYPE STANDARD TABLE OF ty_s_original WITH DEFAULT KEY,

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

      " Consideramos o periodo 2025/06 como vigente, para sempre
      " obter os mesmos resultados nos testes.
      lS_CACHE_seq_to-ano = '2025'."sy-datum(4).
      lS_CACHE_seq_to-mes = '06'. "sy-datum+4(2).

      " @todo Cuidado isto considera datas futuras e
      " isso pode gerar uma quantidade indeterminada de registros.
      if lS_CACHE_seq_to < ls_cache_seq_from.
        lS_CACHE_seq_to = ls_cache_seq_from.
      endif.

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

    METHOD ZIF_BC_CACHE_PERSISTANCE~aggr_or_create_cache_record.

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
        + cast LCL_TYPES=>TY_S_CACHE_AGGR_FIELDS(
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

      lv_REF_T_CACHE_MIN_SEQ_BY_DATA = CAST LCL_TYPES=>TY_T_SORTED_CACHE_SEQ_BY_DATA(
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

class lcl_cache DEFINITION INHERITING FROM ZCL_BC_CACHE_TABLE_INC_AGGR.

  PUBLIC SECTION.

    METHODS:

      lock_cache_records REDEFINITION,

      unlock_cache_records REDEFINITION.

  PROTECTED SECTION.

    METHODS:

      " faz agregação dos valores de iv_ref_s_cache, em iv_ref_s_aggregated_fields.
      aggregate_cache REDEFINITION,
*        IMPORTING
*          iv_ref_s_cache TYPE ty_ref_s_cache
*          iv_ref_s_aggregated_fields TYPE ty_ref_s_cache_aggr_fields,

      " Obtem o nome do ultimo campo do conjunto data da chave do cache.
      get_cache_key_data_last_field REDEFINITION,
*        RETURNING
*          VALUE(RV_RESULT) TYPE string,

      " Elimina os registros que ficaram com delta nulo.
      delete_initial_cache REDEFINITION,
*        IMPORTING
*          iv_ref_t_cache TYPE ty_ref_t_cache,

      " Faz agregação de um registro de cache em outro.
      " Deveriam ter a mesma chave.
      aggregate_cache_to_cache REDEFINITION,
*        IMPORTING
*          iv_ref_s_cache_from type ty_ref_s_cache
*          iv_ref_s_cache_to type ty_ref_s_cache,

      " Faz agregação de um registro original num registro de cache.
      " Deveriam ter a mesma chave.
      aggregate_original_to_cache REDEFINITION,
*        IMPORTING
*          iv_ref_s_original type ty_ref_s_original
*          iv_ref_s_cache type ty_ref_s_cache,

      " Faz a operação inversa da agregação de um registro original
      " num registro de cache.
      " Deveriam ter a mesma chave.
      deaggregate_original_to_cache REDEFINITION,
*        IMPORTING
*          iv_ref_s_original type ty_ref_s_original
*          iv_ref_s_cache type ty_ref_s_cache,

      " Obtém as chaves de um registro original, num registro de cache.
      get_cache_keys_from_orig REDEFINITION.
*        IMPORTING
*          iv_ref_s_original TYPE ty_ref_s_original
*        RETURNING
*          VALUE(rv_result) TYPE ty_ref_s_cache.

ENDCLASS.

CLASS lcl_cache IMPLEMENTATION.

  method lock_cache_records.
  endmethod.

  method unlock_cache_records.
  endmethod.

  method aggregate_cache.

    DATA(lv_ref_s_cache) = CAST LCL_TYPES=>TY_S_CACHE( iv_ref_s_cache ).
    DATA(lv_ref_s_aggregated_fields) = CAST LCL_TYPES=>TY_S_CACHE_AGGR_FIELDS( iv_ref_s_aggregated_fields ).

    lv_ref_s_aggregated_fields->VALOR1 =
      lv_ref_s_aggregated_fields->VALOR1
      + lv_ref_s_cache->AGGR-VALOR1.

  endmethod.

  method get_cache_key_data_last_field.
    RV_RESULT = 'DATA2'.
  endmethod.

  method delete_initial_cache.

    DATA(lv_ref_t_cache) = CAST LCL_TYPES=>TY_T_SORTED_CACHE( iv_ref_t_cache ).

    DELETE lv_ref_t_cache->* WHERE valor1 is INITIAL.

  endmethod.

  method aggregate_cache_to_cache.

    DATA(lv_ref_s_cache_from) = CAST LCL_TYPES=>TY_S_CACHE( iv_ref_s_cache_from ).
    DATA(lv_ref_s_cache_to) = CAST LCL_TYPES=>TY_S_CACHE( iv_ref_s_cache_to ).

    CL_ABAP_UNIT_ASSERT=>ASSERT_EQUALS(
      exporting
        ACT                  = lv_ref_s_cache_from->KEY
        EXP                  = lv_ref_s_cache_to->key
*        IGNORE_HASH_SEQUENCE = ABAP_FALSE    " Ignore sequence in hash tables
*        TOL                  =     " Tolerance Range (for directly passed floating numbers)
        MSG                  = 'As chaves devem ser iguais'    " Description
*        LEVEL                = IF_AUNIT_CONSTANTS=>CRITICAL    " Severity (TOLERABLE, CRITICAL, FATAL)
*        QUIT                 = IF_AUNIT_CONSTANTS=>METHOD    " Alter control flow/ quit test (NO, >METHOD<, CLASS)
*      receiving
*        ASSERTION_FAILED     =     " Condition was not met (and QUIT = NO)
    ).

    lv_ref_s_cache_to->AGGR-VALOR1 =
      lv_ref_s_cache_to->AGGR-VALOR1
      + lv_ref_s_cache_from->AGGR-VALOR1.

  endmethod.

  method aggregate_original_to_cache.

    DATA(lv_ref_s_original) = CAST LCL_TYPES=>TY_S_ORIGINAL( iv_ref_s_original ).
    DATA(lv_ref_s_cache) = CAST LCL_TYPES=>TY_S_CACHE( iv_ref_s_cache ).

    CL_ABAP_UNIT_ASSERT=>ASSERT_EQUALS(
      exporting
        ACT                  = lv_ref_s_original->DATA1
        EXP                  = lv_ref_s_cache->DATA1
*        IGNORE_HASH_SEQUENCE = ABAP_FALSE    " Ignore sequence in hash tables
*        TOL                  =     " Tolerance Range (for directly passed floating numbers)
        MSG                  = 'As chaves devem ser iguais'    " Description
*        LEVEL                = IF_AUNIT_CONSTANTS=>CRITICAL    " Severity (TOLERABLE, CRITICAL, FATAL)
*        QUIT                 = IF_AUNIT_CONSTANTS=>METHOD    " Alter control flow/ quit test (NO, >METHOD<, CLASS)
*      receiving
*        ASSERTION_FAILED     =     " Condition was not met (and QUIT = NO)
    ).

    CL_ABAP_UNIT_ASSERT=>ASSERT_EQUALS(
      exporting
        ACT                  = lv_ref_s_original->DATA2
        EXP                  = lv_ref_s_cache->DATA2
*        IGNORE_HASH_SEQUENCE = ABAP_FALSE    " Ignore sequence in hash tables
*        TOL                  =     " Tolerance Range (for directly passed floating numbers)
        MSG                  = 'As chaves devem ser iguais'    " Description
*        LEVEL                = IF_AUNIT_CONSTANTS=>CRITICAL    " Severity (TOLERABLE, CRITICAL, FATAL)
*        QUIT                 = IF_AUNIT_CONSTANTS=>METHOD    " Alter control flow/ quit test (NO, >METHOD<, CLASS)
*      receiving
*        ASSERTION_FAILED     =     " Condition was not met (and QUIT = NO)
    ).

    CL_ABAP_UNIT_ASSERT=>ASSERT_EQUALS(
      exporting
        ACT                  = lv_ref_s_original->DATE(6)
        EXP                  = |{ lv_ref_s_cache->ANO }{ lv_ref_s_cache->MES }|
*        IGNORE_HASH_SEQUENCE = ABAP_FALSE    " Ignore sequence in hash tables
*        TOL                  =     " Tolerance Range (for directly passed floating numbers)
        MSG                  = 'As chaves devem ser iguais'    " Description
*        LEVEL                = IF_AUNIT_CONSTANTS=>CRITICAL    " Severity (TOLERABLE, CRITICAL, FATAL)
*        QUIT                 = IF_AUNIT_CONSTANTS=>METHOD    " Alter control flow/ quit test (NO, >METHOD<, CLASS)
*      receiving
*        ASSERTION_FAILED     =     " Condition was not met (and QUIT = NO)
    ).

    lv_ref_s_cache->AGGR-VALOR1 =
      lv_ref_s_cache->AGGR-VALOR1
      + LV_REF_S_ORIGINAL->VALOR1.

  endmethod.

  method deaggregate_original_to_cache.

    DATA(lv_ref_s_original) = CAST LCL_TYPES=>TY_S_ORIGINAL( iv_ref_s_original ).
    DATA(LS_ORIGINAL) = lv_ref_s_original->*.

    LS_ORIGINAL-valor1 = - LS_ORIGINAL-valor1.

    me->AGGREGATE_ORIGINAL_TO_CACHE(
      iv_ref_s_original = ref #( LS_ORIGINAL )
      iv_ref_s_cache = iv_ref_s_cache
    ).

  endmethod.

  method get_cache_keys_from_orig.

    DATA:
          lv_ref_s_cache TYPE REF TO LCL_TYPES=>TY_S_CACHE.

    CREATE DATA lv_ref_s_cache.
    RV_RESULT = lv_ref_s_cache.

    DATA(lv_ref_s_original) = CAST LCL_TYPES=>TY_S_ORIGINAL( iv_ref_s_original ).

    LV_REF_S_CACHE->KEY-data1 = lv_ref_s_original->DATA1.
    LV_REF_S_CACHE->KEY-data2 = lv_ref_s_original->DATA2.
    LV_REF_S_CACHE->seq-ano = lv_ref_s_original->date(4).
    LV_REF_S_CACHE->seq-mes = lv_ref_s_original->date+4(2).

  endmethod.

endclass.

CLASS ltcl_test DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.

    METHODS:
      TEST01_2_ins_1_cache for TESTING,

      TEST02_2_ins_2_cache for TESTING,

      TEST03_seq_maior_seq_ate FOR TESTING,

      TEST04_2_ins_10_cache FOR TESTING,

      TEST05_ins_upd FOR TESTING,

      TEST06_ins_del FOR TESTING,

      TEST07_ins_pers_del for TESTING,

      TEST08_complex_cenario for TESTING.

ENDCLASS.

class LTCL_TEST IMPLEMENTATION.

  METHOD TEST01_2_ins_1_cache.

    DATA(LV_PERSISTANCE) = new LCL_PERSISTANCE( VALUE #( ) ).

    DATA(lv_cache) = new lcl_cache(
      IV_FACTORY = new lcl_factory( )
      IV_PERSISTANCE = LV_PERSISTANCE
    ).

    DATA(ls_original_01) = VALUE LCL_TYPES=>TY_S_ORIGINAL(
      DATA0 = '100'
      DATA1 = '100'
      DATA2 = '100'
      DATE = '20250605'
      VALOR1 = 10
    ).

    LV_CACHE->ADD_INSERT( IV_REF_S_ORIGINAL = ref #( ls_original_01 ) ).

    DATA(ls_original_02) = VALUE LCL_TYPES=>TY_S_ORIGINAL(
      DATA0 = '200' " Mudou este valor
      DATA1 = '100'
      DATA2 = '100'
      DATE = '20250605'
      VALOR1 = 10
    ).

    LV_CACHE->ADD_INSERT( IV_REF_S_ORIGINAL = ref #( ls_original_02 ) ).

    LV_CACHE->UPDATE_CACHE( ).

    CL_ABAP_UNIT_ASSERT=>ASSERT_EQUALS(
      exporting
        ACT                  = lines( LV_PERSISTANCE->lt_cache_db_table )    " Data object with current value
        EXP                  = 1    " Data object with expected type
*        IGNORE_HASH_SEQUENCE = ABAP_FALSE    " Ignore sequence in hash tables
*        TOL                  =     " Tolerance Range (for directly passed floating numbers)
        MSG                  = 'Quantidade de entradas no cache distinta da esperada'    " Description
*        LEVEL                = IF_AUNIT_CONSTANTS=>CRITICAL    " Severity (TOLERABLE, CRITICAL, FATAL)
*        QUIT                 = IF_AUNIT_CONSTANTS=>METHOD    " Alter control flow/ quit test (NO, >METHOD<, CLASS)
*      receiving
*        ASSERTION_FAILED     =     " Condition was not met (and QUIT = NO)
    ).

    DATA(ls_cache_exp) = VALUE LCL_TYPES=>TY_S_CACHE(
      DATA1 = '100'
      DATA2 = '100'
      ano = '2025'
      mes = '06'
      VALOR1 = 20
    ).

    CL_ABAP_UNIT_ASSERT=>ASSERT_TABLE_CONTAINS(
      exporting
        LINE             = ls_cache_exp    " Data Object that is typed like line of TABLE
        TABLE            = LV_PERSISTANCE->lt_cache_db_table    " Internal Table that shall contain LINE
        MSG                  = 'O registro de cache esperado não foi encontrado no DB.'    " Description
*        LEVEL            = IF_AUNIT_CONSTANTS=>CRITICAL    " Severity (TOLERABLE, >CRITICAL<, FATAL)
*        QUIT             = IF_AUNIT_CONSTANTS=>METHOD    " Alter control flow/ quit test (NO, >METHOD<, CLASS)
*      receiving
*        ASSERTION_FAILED =     " Condition was not met (and QUIT = NO)
    ).

  ENDMETHOD.

  METHOD TEST02_2_ins_2_cache.

    DATA(LV_PERSISTANCE) = new LCL_PERSISTANCE( VALUE #( ) ).

    DATA(lv_cache) = new lcl_cache(
      IV_FACTORY = new lcl_factory( )
      IV_PERSISTANCE = LV_PERSISTANCE
    ).

    DATA(ls_original_01) = VALUE LCL_TYPES=>TY_S_ORIGINAL(
      DATA0 = '100'
      DATA1 = '100'
      DATA2 = '100'
      DATE = '20250605'
      VALOR1 = 10
    ).

    LV_CACHE->ADD_INSERT( IV_REF_S_ORIGINAL = ref #( ls_original_01 ) ).

    DATA(ls_original_02) = VALUE LCL_TYPES=>TY_S_ORIGINAL(
      DATA0 = '100'
      DATA1 = '200' " Mudou a chave
      DATA2 = '100'
      DATE = '20250605'
      VALOR1 = 20
    ).

    LV_CACHE->ADD_INSERT( IV_REF_S_ORIGINAL = ref #( ls_original_02 ) ).

    LV_CACHE->UPDATE_CACHE( ).

    CL_ABAP_UNIT_ASSERT=>ASSERT_EQUALS(
      exporting
        ACT                  = lines( LV_PERSISTANCE->lt_cache_db_table )    " Data object with current value
        EXP                  = 2    " Data object with expected type
*        IGNORE_HASH_SEQUENCE = ABAP_FALSE    " Ignore sequence in hash tables
*        TOL                  =     " Tolerance Range (for directly passed floating numbers)
        MSG                  = 'Quantidade de entradas no cache distinta da esperada'    " Description
*        LEVEL                = IF_AUNIT_CONSTANTS=>CRITICAL    " Severity (TOLERABLE, CRITICAL, FATAL)
*        QUIT                 = IF_AUNIT_CONSTANTS=>METHOD    " Alter control flow/ quit test (NO, >METHOD<, CLASS)
*      receiving
*        ASSERTION_FAILED     =     " Condition was not met (and QUIT = NO)
    ).

    CL_ABAP_UNIT_ASSERT=>ASSERT_TABLE_CONTAINS(
      exporting
        LINE             = VALUE LCL_TYPES=>TY_S_CACHE(
          DATA1 = '100'
          DATA2 = '100'
          ano = '2025'
          mes = '06'
          VALOR1 = 10
        )
        TABLE            = LV_PERSISTANCE->lt_cache_db_table    " Internal Table that shall contain LINE
        MSG                  = 'O registro de cache esperado não foi encontrado no DB.'    " Description
*        LEVEL            = IF_AUNIT_CONSTANTS=>CRITICAL    " Severity (TOLERABLE, >CRITICAL<, FATAL)
*        QUIT             = IF_AUNIT_CONSTANTS=>METHOD    " Alter control flow/ quit test (NO, >METHOD<, CLASS)
*      receiving
*        ASSERTION_FAILED =     " Condition was not met (and QUIT = NO)
    ).

    CL_ABAP_UNIT_ASSERT=>ASSERT_TABLE_CONTAINS(
      exporting
        LINE             = VALUE LCL_TYPES=>TY_S_CACHE(
          DATA1 = '200'
          DATA2 = '100'
          ano = '2025'
          mes = '06'
          VALOR1 = 20
        )
        TABLE            = LV_PERSISTANCE->lt_cache_db_table    " Internal Table that shall contain LINE
        MSG                  = 'O registro de cache esperado não foi encontrado no DB.'    " Description
*        LEVEL            = IF_AUNIT_CONSTANTS=>CRITICAL    " Severity (TOLERABLE, >CRITICAL<, FATAL)
*        QUIT             = IF_AUNIT_CONSTANTS=>METHOD    " Alter control flow/ quit test (NO, >METHOD<, CLASS)
*      receiving
*        ASSERTION_FAILED =     " Condition was not met (and QUIT = NO)
    ).

  ENDMETHOD.


  METHOD TEST03_seq_maior_seq_ate.

    DATA(LV_PERSISTANCE) = new LCL_PERSISTANCE( VALUE #( ) ).

    DATA(lv_cache) = new lcl_cache(
      IV_FACTORY = new lcl_factory( )
      IV_PERSISTANCE = LV_PERSISTANCE
    ).

    DATA(ls_original_02) = VALUE LCL_TYPES=>TY_S_ORIGINAL(
      DATA0 = '100'
      DATA1 = '100'
      DATA2 = '100'
      DATE = '20250705' " Mudou o mês
      VALOR1 = 20
    ).

    LV_CACHE->ADD_INSERT( IV_REF_S_ORIGINAL = ref #( ls_original_02 ) ).

    LV_CACHE->UPDATE_CACHE( ).

    CL_ABAP_UNIT_ASSERT=>ASSERT_EQUALS(
      exporting
        ACT                  = lines( LV_PERSISTANCE->lt_cache_db_table )    " Data object with current value
        EXP                  = 1    " Data object with expected type
*        IGNORE_HASH_SEQUENCE = ABAP_FALSE    " Ignore sequence in hash tables
*        TOL                  =     " Tolerance Range (for directly passed floating numbers)
        MSG                  = 'Quantidade de entradas no cache distinta da esperada'    " Description
        LEVEL                = IF_AUNIT_CONSTANTS=>TOLERABLE    " Severity (TOLERABLE, CRITICAL, FATAL)
*        QUIT                 = IF_AUNIT_CONSTANTS=>METHOD    " Alter control flow/ quit test (NO, >METHOD<, CLASS)
*      receiving
*        ASSERTION_FAILED     =     " Condition was not met (and QUIT = NO)
    ).

    CL_ABAP_UNIT_ASSERT=>ASSERT_TABLE_CONTAINS(
      exporting
        LINE             = VALUE LCL_TYPES=>TY_S_CACHE(
          DATA1 = '100'
          DATA2 = '100'
          ano = '2025'
          mes = '07'
          VALOR1 = 20
        )
        TABLE            = LV_PERSISTANCE->lt_cache_db_table    " Internal Table that shall contain LINE
        MSG                  = 'O registro de cache esperado não foi encontrado no DB.'    " Description
*        LEVEL            = IF_AUNIT_CONSTANTS=>CRITICAL    " Severity (TOLERABLE, >CRITICAL<, FATAL)
*        QUIT             = IF_AUNIT_CONSTANTS=>METHOD    " Alter control flow/ quit test (NO, >METHOD<, CLASS)
*      receiving
*        ASSERTION_FAILED =     " Condition was not met (and QUIT = NO)
    ).
  ENDMETHOD.

  METHOD TEST04_2_ins_10_cache.

    DATA(LV_PERSISTANCE) = new LCL_PERSISTANCE( VALUE #( ) ).

    DATA(lv_cache) = new lcl_cache(
      IV_FACTORY = new lcl_factory( )
      IV_PERSISTANCE = LV_PERSISTANCE
    ).

    DATA(ls_original_01) = VALUE LCL_TYPES=>TY_S_ORIGINAL(
      DATA0 = '100'
      DATA1 = '100'
      DATA2 = '100'
      DATE = '20240905'
      VALOR1 = 10
    ).

    LV_CACHE->ADD_INSERT( IV_REF_S_ORIGINAL = ref #( ls_original_01 ) ).

    DATA(ls_original_02) = VALUE LCL_TYPES=>TY_S_ORIGINAL(
      DATA0 = '100'
      DATA1 = '100'
      DATA2 = '100'
      DATE = '20250305'
      VALOR1 = 20
    ).

    LV_CACHE->ADD_INSERT( IV_REF_S_ORIGINAL = ref #( ls_original_02 ) ).

    LV_CACHE->UPDATE_CACHE( ).

    CL_ABAP_UNIT_ASSERT=>ASSERT_EQUALS(
      exporting
        ACT                  = lines( LV_PERSISTANCE->lt_cache_db_table )    " Data object with current value
        EXP                  = 10    " Data object with expected type
*        IGNORE_HASH_SEQUENCE = ABAP_FALSE    " Ignore sequence in hash tables
*        TOL                  =     " Tolerance Range (for directly passed floating numbers)
        MSG                  = 'Quantidade de entradas no cache distinta da esperada'    " Description
*        LEVEL                = IF_AUNIT_CONSTANTS=>CRITICAL    " Severity (TOLERABLE, CRITICAL, FATAL)
*        QUIT                 = IF_AUNIT_CONSTANTS=>METHOD    " Alter control flow/ quit test (NO, >METHOD<, CLASS)
*      receiving
*        ASSERTION_FAILED     =     " Condition was not met (and QUIT = NO)
    ).

    CL_ABAP_UNIT_ASSERT=>ASSERT_TABLE_CONTAINS(
      exporting
        LINE             = VALUE LCL_TYPES=>TY_S_CACHE(
          DATA1 = '100'
          DATA2 = '100'
          ano = '2024'
          mes = '09'
          VALOR1 = 10
        )
        TABLE            = LV_PERSISTANCE->lt_cache_db_table    " Internal Table that shall contain LINE
        MSG                  = 'O registro de cache esperado não foi encontrado no DB.'    " Description
*        LEVEL            = IF_AUNIT_CONSTANTS=>CRITICAL    " Severity (TOLERABLE, >CRITICAL<, FATAL)
*        QUIT             = IF_AUNIT_CONSTANTS=>METHOD    " Alter control flow/ quit test (NO, >METHOD<, CLASS)
*      receiving
*        ASSERTION_FAILED =     " Condition was not met (and QUIT = NO)
    ).

    CL_ABAP_UNIT_ASSERT=>ASSERT_TABLE_CONTAINS(
      exporting
        LINE             = VALUE LCL_TYPES=>TY_S_CACHE(
          DATA1 = '100'
          DATA2 = '100'
          ano = '2025'
          mes = '02'
          VALOR1 = 10
        )
        TABLE            = LV_PERSISTANCE->lt_cache_db_table    " Internal Table that shall contain LINE
        MSG                  = 'O registro de cache esperado não foi encontrado no DB.'    " Description
*        LEVEL            = IF_AUNIT_CONSTANTS=>CRITICAL    " Severity (TOLERABLE, >CRITICAL<, FATAL)
*        QUIT             = IF_AUNIT_CONSTANTS=>METHOD    " Alter control flow/ quit test (NO, >METHOD<, CLASS)
*      receiving
*        ASSERTION_FAILED =     " Condition was not met (and QUIT = NO)
    ).

    CL_ABAP_UNIT_ASSERT=>ASSERT_TABLE_CONTAINS(
      exporting
        LINE             = VALUE LCL_TYPES=>TY_S_CACHE(
          DATA1 = '100'
          DATA2 = '100'
          ano = '2025'
          mes = '03'
          VALOR1 = 30
        )
        TABLE            = LV_PERSISTANCE->lt_cache_db_table    " Internal Table that shall contain LINE
        MSG                  = 'O registro de cache esperado não foi encontrado no DB.'    " Description
*        LEVEL            = IF_AUNIT_CONSTANTS=>CRITICAL    " Severity (TOLERABLE, >CRITICAL<, FATAL)
*        QUIT             = IF_AUNIT_CONSTANTS=>METHOD    " Alter control flow/ quit test (NO, >METHOD<, CLASS)
*      receiving
*        ASSERTION_FAILED =     " Condition was not met (and QUIT = NO)
    ).

    CL_ABAP_UNIT_ASSERT=>ASSERT_TABLE_CONTAINS(
      exporting
        LINE             = VALUE LCL_TYPES=>TY_S_CACHE(
          DATA1 = '100'
          DATA2 = '100'
          ano = '2025'
          mes = '06'
          VALOR1 = 30
        )
        TABLE            = LV_PERSISTANCE->lt_cache_db_table    " Internal Table that shall contain LINE
        MSG                  = 'O registro de cache esperado não foi encontrado no DB.'    " Description
*        LEVEL            = IF_AUNIT_CONSTANTS=>CRITICAL    " Severity (TOLERABLE, >CRITICAL<, FATAL)
*        QUIT             = IF_AUNIT_CONSTANTS=>METHOD    " Alter control flow/ quit test (NO, >METHOD<, CLASS)
*      receiving
*        ASSERTION_FAILED =     " Condition was not met (and QUIT = NO)
    ).


  ENDMETHOD.

  METHOD TEST05_ins_upd.

    DATA(LV_PERSISTANCE) = new LCL_PERSISTANCE( VALUE #( ) ).

    DATA(lv_cache) = new lcl_cache(
      IV_FACTORY = new lcl_factory( )
      IV_PERSISTANCE = LV_PERSISTANCE
    ).

    DATA(ls_original_01) = VALUE LCL_TYPES=>TY_S_ORIGINAL(
      DATA0 = '100'
      DATA1 = '100'
      DATA2 = '100'
      DATE = '20250605' " Mudou o mês
      VALOR1 = 20
    ).

    LV_CACHE->ADD_INSERT( IV_REF_S_ORIGINAL = ref #( ls_original_01 ) ).

    DATA(ls_original_02) = ls_original_01.
    ls_original_02-VALOR1 = 10.

    LV_CACHE->ADD_UPDATE(
      IV_REF_S_ORIGINAL_OLD = ref #( ls_original_01 )
      IV_REF_S_ORIGINAL_new = ref #( ls_original_02 )
    ).

    LV_CACHE->UPDATE_CACHE( ).

    CL_ABAP_UNIT_ASSERT=>ASSERT_EQUALS(
      exporting
        ACT                  = lines( LV_PERSISTANCE->lt_cache_db_table )    " Data object with current value
        EXP                  = 1    " Data object with expected type
*        IGNORE_HASH_SEQUENCE = ABAP_FALSE    " Ignore sequence in hash tables
*        TOL                  =     " Tolerance Range (for directly passed floating numbers)
        MSG                  = 'Quantidade de entradas no cache distinta da esperada'    " Description
        LEVEL                = IF_AUNIT_CONSTANTS=>TOLERABLE    " Severity (TOLERABLE, CRITICAL, FATAL)
*        QUIT                 = IF_AUNIT_CONSTANTS=>METHOD    " Alter control flow/ quit test (NO, >METHOD<, CLASS)
*      receiving
*        ASSERTION_FAILED     =     " Condition was not met (and QUIT = NO)
    ).

    CL_ABAP_UNIT_ASSERT=>ASSERT_TABLE_CONTAINS(
      exporting
        LINE             = VALUE LCL_TYPES=>TY_S_CACHE(
          DATA1 = '100'
          DATA2 = '100'
          ano = '2025'
          mes = '06'
          VALOR1 = 10
        )
        TABLE            = LV_PERSISTANCE->lt_cache_db_table    " Internal Table that shall contain LINE
        MSG                  = 'O registro de cache esperado não foi encontrado no DB.'    " Description
*        LEVEL            = IF_AUNIT_CONSTANTS=>CRITICAL    " Severity (TOLERABLE, >CRITICAL<, FATAL)
*        QUIT             = IF_AUNIT_CONSTANTS=>METHOD    " Alter control flow/ quit test (NO, >METHOD<, CLASS)
*      receiving
*        ASSERTION_FAILED =     " Condition was not met (and QUIT = NO)
    ).

  ENDMETHOD.

  METHOD TEST06_ins_del.

    DATA(LV_PERSISTANCE) = new LCL_PERSISTANCE( VALUE #( ) ).

    DATA(lv_cache) = new lcl_cache(
      IV_FACTORY = new lcl_factory( )
      IV_PERSISTANCE = LV_PERSISTANCE
    ).

    DATA(ls_original_01) = VALUE LCL_TYPES=>TY_S_ORIGINAL(
      DATA0 = '100'
      DATA1 = '100'
      DATA2 = '100'
      DATE = '20250605' " Mudou o mês
      VALOR1 = 20
    ).

    LV_CACHE->ADD_INSERT( IV_REF_S_ORIGINAL = ref #( ls_original_01 ) ).

    LV_CACHE->ADD_DELETE(
      IV_REF_S_ORIGINAL = REF #( ls_original_01 )
    ).

    LV_CACHE->UPDATE_CACHE( ).

    CL_ABAP_UNIT_ASSERT=>ASSERT_EQUALS(
      exporting
        ACT                  = lines( LV_PERSISTANCE->lt_cache_db_table )    " Data object with current value
        EXP                  = 0    " Data object with expected type
*        IGNORE_HASH_SEQUENCE = ABAP_FALSE    " Ignore sequence in hash tables
*        TOL                  =     " Tolerance Range (for directly passed floating numbers)
        MSG                  = 'Quantidade de entradas no cache distinta da esperada'    " Description
        LEVEL                = IF_AUNIT_CONSTANTS=>CRITICAL    " Severity (TOLERABLE, CRITICAL, FATAL)
*        QUIT                 = IF_AUNIT_CONSTANTS=>METHOD    " Alter control flow/ quit test (NO, >METHOD<, CLASS)
*      receiving
*        ASSERTION_FAILED     =     " Condition was not met (and QUIT = NO)
    ).

  ENDMETHOD.

  METHOD TEST07_ins_pers_del.

    DATA(LV_PERSISTANCE) = new LCL_PERSISTANCE( VALUE #( ) ).

    DATA(lv_cache) = new lcl_cache(
      IV_FACTORY = new lcl_factory( )
      IV_PERSISTANCE = LV_PERSISTANCE
    ).

    DATA(ls_original_01) = VALUE LCL_TYPES=>TY_S_ORIGINAL(
      DATA0 = '100'
      DATA1 = '100'
      DATA2 = '100'
      DATE = '20250605' " Mudou o mês
      VALOR1 = 20
    ).

    LV_CACHE->ADD_INSERT( IV_REF_S_ORIGINAL = ref #( ls_original_01 ) ).

    LV_CACHE->UPDATE_CACHE( ).

    DATA(LV_PERSISTANCE_02) = new LCL_PERSISTANCE(
      LV_PERSISTANCE->lt_cache_db_table
    ).

    lv_cache = new lcl_cache(
      IV_FACTORY = new lcl_factory( )
      IV_PERSISTANCE = LV_PERSISTANCE_02
    ).

    LV_CACHE->ADD_DELETE(
      IV_REF_S_ORIGINAL = REF #( ls_original_01 )
    ).

    LV_CACHE->UPDATE_CACHE( ).

    CL_ABAP_UNIT_ASSERT=>ASSERT_EQUALS(
      exporting
        ACT                  = lines( LV_PERSISTANCE_02->lt_cache_db_table )    " Data object with current value
        EXP                  = 1    " Data object with expected type
*        IGNORE_HASH_SEQUENCE = ABAP_FALSE    " Ignore sequence in hash tables
*        TOL                  =     " Tolerance Range (for directly passed floating numbers)
        MSG                  = 'Quantidade de entradas no cache distinta da esperada'    " Description
        LEVEL                = IF_AUNIT_CONSTANTS=>CRITICAL    " Severity (TOLERABLE, CRITICAL, FATAL)
*        QUIT                 = IF_AUNIT_CONSTANTS=>METHOD    " Alter control flow/ quit test (NO, >METHOD<, CLASS)
*      receiving
*        ASSERTION_FAILED     =     " Condition was not met (and QUIT = NO)
    ).

    CL_ABAP_UNIT_ASSERT=>ASSERT_TABLE_CONTAINS(
      exporting
        LINE             = VALUE LCL_TYPES=>TY_S_CACHE(
          DATA1 = '100'
          DATA2 = '100'
          ano = '2025'
          mes = '06'
          VALOR1 = 0
        )
        TABLE            = LV_PERSISTANCE_02->lt_cache_db_table    " Internal Table that shall contain LINE
        MSG                  = 'O registro de cache esperado não foi encontrado no DB.'    " Description
*        LEVEL            = IF_AUNIT_CONSTANTS=>CRITICAL    " Severity (TOLERABLE, >CRITICAL<, FATAL)
*        QUIT             = IF_AUNIT_CONSTANTS=>METHOD    " Alter control flow/ quit test (NO, >METHOD<, CLASS)
*      receiving
*        ASSERTION_FAILED =     " Condition was not met (and QUIT = NO)
    ).

  ENDMETHOD.

  METHOD TEST08_complex_cenario.

    " 100 100 100 (insert)
      " 202503
        " 20
      " 202505
        " 200
    " 200 100 100 (insert)
      " 202503
        " 100
    " ->
      " 100 100
        " 202503 120
        " 202504 120
        " 202505 320
        " 202506 320

    " 100 100 200 (insert)
      " 202412
        " 30
      " 202501
        " 200
      " 202504
        " 5
    " ->
      " 100 200
        " 202412 30
        " 202501 230
        " 202502 230
        " 202503 230
        " 202504 235
        " 202505 235
        " 202506 235

    data(lt_insert_original_01) = VALUE LCL_TYPES=>TY_T_ORIGINAL(
      ( DATA0 = '100' DATA1 = '100' DATA2 = '100' DATE = '20250302' VALOR1 = 20 )
      ( DATA0 = '100' DATA1 = '100' DATA2 = '100' DATE = '20250504' VALOR1 = 200 )
      ( DATA0 = '200' DATA1 = '100' DATA2 = '100' DATE = '20250312' VALOR1 = 100 )
      ( DATA0 = '100' DATA1 = '100' DATA2 = '200' DATE = '20241220' VALOR1 = 30 )
      ( DATA0 = '100' DATA1 = '100' DATA2 = '200' DATE = '20250119' VALOR1 = 200 )
      ( DATA0 = '100' DATA1 = '100' DATA2 = '200' DATE = '20250415' VALOR1 = 5 )
    ).

    DATA(lt_expected_cache_01) = VALUE LCL_TYPES=>TY_T_SORTED_CACHE(
      ( DATA1 = '100' DATA2 = '100' ano = '2025' mes = '03' VALOR1 = 120 )
      ( DATA1 = '100' DATA2 = '100' ano = '2025' mes = '04' VALOR1 = 120 )
      ( DATA1 = '100' DATA2 = '100' ano = '2025' mes = '05' VALOR1 = 320 )
      ( DATA1 = '100' DATA2 = '100' ano = '2025' mes = '06' VALOR1 = 320 )

      ( DATA1 = '100' DATA2 = '200' ano = '2024' mes = '12' VALOR1 = 30 )
      ( DATA1 = '100' DATA2 = '200' ano = '2025' mes = '01' VALOR1 = 230 )
      ( DATA1 = '100' DATA2 = '200' ano = '2025' mes = '02' VALOR1 = 230 )
      ( DATA1 = '100' DATA2 = '200' ano = '2025' mes = '03' VALOR1 = 230 )
      ( DATA1 = '100' DATA2 = '200' ano = '2025' mes = '04' VALOR1 = 235 )
      ( DATA1 = '100' DATA2 = '200' ano = '2025' mes = '05' VALOR1 = 235 )
      ( DATA1 = '100' DATA2 = '200' ano = '2025' mes = '06' VALOR1 = 235 )
    ).

    DATA(LV_PERSISTANCE) = new LCL_PERSISTANCE( VALUE #( ) ).

    DATA(lv_cache) = new lcl_cache(
      IV_FACTORY = new lcl_factory( )
      IV_PERSISTANCE = LV_PERSISTANCE
    ).

    LOOP AT LT_INSERT_ORIGINAL_01 ASSIGNING FIELD-SYMBOL(<LS_INSERT_ORIGINAL_01>).
      LV_CACHE->ADD_INSERT( IV_REF_S_ORIGINAL = ref #( <LS_INSERT_ORIGINAL_01> ) ).
    ENDLOOP.

    LV_CACHE->UPDATE_CACHE( ).

    CL_ABAP_UNIT_ASSERT=>ASSERT_EQUALS(
      exporting
        ACT                  = lines( LV_PERSISTANCE->lt_cache_db_table )    " Data object with current value
        EXP                  = lines( lt_expected_cache_01 )    " Data object with expected type
*        IGNORE_HASH_SEQUENCE = ABAP_FALSE    " Ignore sequence in hash tables
*        TOL                  =     " Tolerance Range (for directly passed floating numbers)
        MSG                  = 'Quantidade de entradas no cache distinta da esperada'    " Description
        LEVEL                = IF_AUNIT_CONSTANTS=>CRITICAL    " Severity (TOLERABLE, CRITICAL, FATAL)
*        QUIT                 = IF_AUNIT_CONSTANTS=>METHOD    " Alter control flow/ quit test (NO, >METHOD<, CLASS)
*      receiving
*        ASSERTION_FAILED     =     " Condition was not met (and QUIT = NO)
    ).

    LOOP AT lt_expected_cache_01 ASSIGNING FIELD-SYMBOL(<ls_expected_cache_01>).

      CL_ABAP_UNIT_ASSERT=>ASSERT_TABLE_CONTAINS(
        exporting
          LINE             = <ls_expected_cache_01>
          TABLE            = LV_PERSISTANCE->lt_cache_db_table    " Internal Table that shall contain LINE
          MSG                  = 'O registro de cache esperado não foi encontrado no DB.'    " Description
*          LEVEL            = IF_AUNIT_CONSTANTS=>CRITICAL    " Severity (TOLERABLE, >CRITICAL<, FATAL)
*          QUIT             = IF_AUNIT_CONSTANTS=>METHOD    " Alter control flow/ quit test (NO, >METHOD<, CLASS)
*        receiving
*          ASSERTION_FAILED =     " Condition was not met (and QUIT = NO)
      ).

    ENDLOOP.

    "Cache
      " 100 100
        " 202503 120
        " 202504 120
        " 202505 320
        " 202506 320
      " 100 200
        " 202412 30
        " 202501 230
        " 202502 230
        " 202503 230
        " 202504 235
        " 202505 235
        " 202506 235

    data(lt_insert_original_02) = VALUE LCL_TYPES=>TY_T_ORIGINAL(
      ( DATA0 = '100' DATA1 = '100' DATA2 = '100' DATE = '20250402' VALOR1 = 18 )
      ( DATA0 = '200' DATA1 = '100' DATA2 = '200' DATE = '20250119' VALOR1 = 33 )
    ).

    data(lt_update_original_02) = VALUE LCL_TYPES=>TY_T_ORIGINAL(
      ( DATA0 = '100' DATA1 = '100' DATA2 = '100' DATE = '20250302' VALOR1 = 20 )
      ( DATA0 = '100' DATA1 = '100' DATA2 = '100' DATE = '20250402' VALOR1 = 21 ) " Cambia periodo e valor
    ).

    data(lt_delete_original_02) = VALUE LCL_TYPES=>TY_T_ORIGINAL(
      ( DATA0 = '100' DATA1 = '100' DATA2 = '100' DATE = '20250504' VALOR1 = 200 )
    ).

    "Cache
      " 100 100
        " 202503 100
        " 202504 139
        " 202505 139
        " 202506 139
      " 100 200
        " 202412 30
        " 202501 263
        " 202502 263
        " 202503 263
        " 202504 268
        " 202505 268
        " 202506 268

    DATA(lt_expected_cache_02) = VALUE LCL_TYPES=>TY_T_SORTED_CACHE(
      ( DATA1 = '100' DATA2 = '100' ano = '2025' mes = '03' VALOR1 = 100 )
      ( DATA1 = '100' DATA2 = '100' ano = '2025' mes = '04' VALOR1 = 139 )
      ( DATA1 = '100' DATA2 = '100' ano = '2025' mes = '05' VALOR1 = 139 )
      ( DATA1 = '100' DATA2 = '100' ano = '2025' mes = '06' VALOR1 = 139 )

      ( DATA1 = '100' DATA2 = '200' ano = '2024' mes = '12' VALOR1 = 30 )
      ( DATA1 = '100' DATA2 = '200' ano = '2025' mes = '01' VALOR1 = 263 )
      ( DATA1 = '100' DATA2 = '200' ano = '2025' mes = '02' VALOR1 = 263 )
      ( DATA1 = '100' DATA2 = '200' ano = '2025' mes = '03' VALOR1 = 263 )
      ( DATA1 = '100' DATA2 = '200' ano = '2025' mes = '04' VALOR1 = 268 )
      ( DATA1 = '100' DATA2 = '200' ano = '2025' mes = '05' VALOR1 = 268 )
      ( DATA1 = '100' DATA2 = '200' ano = '2025' mes = '06' VALOR1 = 268 )
    ).


    DATA(LV_PERSISTANCE_02) = new LCL_PERSISTANCE( LV_PERSISTANCE->lt_cache_db_table ).

    DATA(lv_cache_02) = new lcl_cache(
      IV_FACTORY = new lcl_factory( )
      IV_PERSISTANCE = LV_PERSISTANCE_02
    ).

    LOOP AT LT_INSERT_ORIGINAL_02 ASSIGNING FIELD-SYMBOL(<LS_INSERT_ORIGINAL_02>).
      LV_CACHE_02->ADD_INSERT( IV_REF_S_ORIGINAL = ref #( <LS_INSERT_ORIGINAL_02> ) ).
    ENDLOOP.

    DATA(lv_updates_count) = lines( LT_update_ORIGINAL_02 ) / 2.

    DO lv_updates_count TIMES.

      DATA(lv_index) = sy-index.
      LV_CACHE_02->ADD_update(
        IV_REF_S_ORIGINAL_OLD = ref #( LT_UPDATE_ORIGINAL_02[ lv_index ] )
        IV_REF_S_ORIGINAL_new = ref #( LT_UPDATE_ORIGINAL_02[ lv_index + 1 ] )
      ).

    ENDDO.

    LOOP AT LT_delete_ORIGINAL_02 ASSIGNING FIELD-SYMBOL(<LS_delete_ORIGINAL_02>).
      LV_CACHE_02->ADD_delete( IV_REF_S_ORIGINAL = ref #( <LS_delete_ORIGINAL_02> ) ).
    ENDLOOP.

    LV_CACHE_02->UPDATE_CACHE( ).

    CL_ABAP_UNIT_ASSERT=>ASSERT_EQUALS(
      exporting
        ACT                  = lines( LV_PERSISTANCE_02->lt_cache_db_table )    " Data object with current value
        EXP                  = lines( lt_expected_cache_02 )    " Data object with expected type
*        IGNORE_HASH_SEQUENCE = ABAP_FALSE    " Ignore sequence in hash tables
*        TOL                  =     " Tolerance Range (for directly passed floating numbers)
        MSG                  = 'Quantidade de entradas no cache distinta da esperada'    " Description
        LEVEL                = IF_AUNIT_CONSTANTS=>CRITICAL    " Severity (TOLERABLE, CRITICAL, FATAL)
*        QUIT                 = IF_AUNIT_CONSTANTS=>METHOD    " Alter control flow/ quit test (NO, >METHOD<, CLASS)
*      receiving
*        ASSERTION_FAILED     =     " Condition was not met (and QUIT = NO)
    ).

    LOOP AT lt_expected_cache_02 ASSIGNING FIELD-SYMBOL(<ls_expected_cache_02>).

      CL_ABAP_UNIT_ASSERT=>ASSERT_TABLE_CONTAINS(
        exporting
          LINE             = <ls_expected_cache_02>
          TABLE            = LV_PERSISTANCE_02->lt_cache_db_table    " Internal Table that shall contain LINE
          MSG                  = 'O registro de cache esperado não foi encontrado no DB.'    " Description
*          LEVEL            = IF_AUNIT_CONSTANTS=>CRITICAL    " Severity (TOLERABLE, >CRITICAL<, FATAL)
*          QUIT             = IF_AUNIT_CONSTANTS=>METHOD    " Alter control flow/ quit test (NO, >METHOD<, CLASS)
*        receiving
*          ASSERTION_FAILED =     " Condition was not met (and QUIT = NO)
      ).

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
