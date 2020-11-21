%%--------------------------------------------------------------------
%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-ifndef(_DOLPHINC_HRL).
-define(_DOLPHINC_HRL, true).

%% Data Type
-define(DT_VOID,           0).
-define(DT_BOOL,           1).
-define(DT_CHAR,           2).
-define(DT_SHORT,          3).
-define(DT_INT,            4).
-define(DT_LONG,           5).
-define(DT_DATE,           6).
-define(DT_MONTH,          7).
-define(DT_TIME,           8).
-define(DT_MINUTE,         9).
-define(DT_SECOND,        10).
-define(DT_DATETIME,      11).
-define(DT_TIMESTAMP,     12).
-define(DT_NANOTIME,      13).
-define(DT_NANOTIMESTAMP, 14).
-define(DT_FLOAT,         15).
-define(DT_DOUBLE,        16).
-define(DT_SYMBOL,        17).
-define(DT_STRING,        18).
-define(DT_UUID,          19).
-define(DT_FUNCTIONDEF,   20).
-define(DT_HANDLE,        21).
-define(DT_CODE,          22).
-define(DT_DATASOURCE,    23).
-define(DT_RESOURCE,      24).
-define(DT_ANY,           25).
-define(DT_COMPRESS,      26).
-define(DT_DICTIONARY,    27).
-define(DT_DATEHOUR,      28).
-define(DT_IPADDR,        30).
-define(DT_INT128,        31).

%% Data From
-define(DF_SCALAR,         0).
-define(DF_VECTOR,         1).
-define(DF_PAIR,           2).
-define(DF_MATRIX,         3).
-define(DF_SET,            4).
-define(DF_DICTIONARY,     5).
-define(DF_TABLE,          6).
-define(DF_CHART,          7).
-define(DF_CHUNK,          8).

-endif.
