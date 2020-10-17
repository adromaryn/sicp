[{"kind":2,"name":"Founder::Payment::Receipt","line":0,"defintion":1},{"name":"strict","line":2,"kind":2,"containerName":""},{"containerName":"","kind":2,"line":3,"name":"warnings"},{"containerName":"","name":"utf8","line":4,"kind":2},{"containerName":"MR","line":6,"name":"Log","kind":2},{"containerName":"MR","kind":2,"name":"ActiveRecordPg","line":7},{"name":"Type","line":8,"kind":2,"containerName":"MR::ActiveRecordPg"},{"containerName":"Founder","kind":2,"name":"Config","line":10},{"name":"Payment","line":11,"kind":2,"containerName":"Founder"},{"containerName":"DMR::Fiscal","line":13,"name":"Receipt","kind":2},{"kind":2,"line":14,"name":"Type","containerName":"DMR"},{"kind":12,"line":16,"name":"db"},{"containerName":"Config::db_pg","line":16,"name":"Founder","kind":12},{"kind":12,"line":17,"name":"table"},{"kind":14,"line":19,"name":"FATAL_TIMEOUT","defintion":1,"containerName":"Founder::Payment::Receipt"},{"containerName":"Founder::Payment::Receipt","line":20,"name":"RECHECK_INTERVAL","kind":14,"defintion":1},{"kind":2,"name":"overload","line":23,"containerName":""},{"kind":12,"line":24,"name":"fallback"},{"name":"has_field","line":26,"kind":12},{"kind":12,"name":"id","line":26},{"line":26,"name":"is","kind":12},{"kind":12,"name":"isa","line":26},{"kind":12,"line":26,"name":"selector"},{"name":"primary_key","line":26,"kind":12},{"kind":12,"name":"required","line":26},{"name":"has_field","line":27,"kind":12},{"line":27,"name":"issuer_id","kind":12},{"kind":12,"line":27,"name":"is"},{"kind":12,"name":"isa","line":27},{"name":"required","line":27,"kind":12},{"name":"has_field","line":28,"kind":12},{"name":"receipt_id","line":28,"kind":12},{"kind":12,"name":"is","line":28},{"kind":12,"name":"isa","line":28},{"kind":12,"line":28,"name":"selector"},{"line":28,"name":"uniq","kind":12},{"kind":12,"name":"has_field","line":29},{"kind":12,"name":"status","line":29},{"kind":12,"line":29,"name":"is"},{"kind":12,"name":"isa","line":29},{"line":29,"name":"required","kind":12},{"kind":12,"name":"default","line":29},{"kind":12,"line":30,"name":"has_field"},{"kind":12,"name":"create_at","line":30},{"kind":12,"name":"is","line":30},{"name":"isa","line":30,"kind":12},{"line":30,"name":"default","kind":12},{"kind":12,"line":30,"name":"time"},{"name":"has_field","line":31,"kind":12},{"line":31,"name":"last_check_time","kind":12},{"name":"is","line":31,"kind":12},{"line":31,"name":"isa","kind":12},{"name":"payment","line":33,"kind":12},{"name":"is","line":34,"kind":12},{"kind":12,"line":35,"name":"isa"},{"name":"lazy","line":36,"kind":12},{"line":37,"name":"default","kind":12},{"name":"Founder","line":37,"kind":12,"containerName":"Payment"},{"containerName":"Founder::Payment::Receipt","defintion":"sub","name":"select_by_id","kind":12,"line":37,"children":[]},{"kind":12,"line":37,"name":"id","containerName":"Founder::Payment::Receipt"},{"line":40,"children":[{"name":"$self","line":41,"kind":13,"defintion":"my","containerName":"BUILDARGS"},{"kind":13,"line":41,"name":"%args","containerName":"BUILDARGS"},{"containerName":"BUILDARGS","name":"$args","line":42,"kind":13},{"containerName":"BUILDARGS","kind":13,"name":"$args","line":43},{"name":"$args","line":43,"kind":13,"containerName":"BUILDARGS"},{"containerName":"BUILDARGS","kind":13,"line":44,"name":"$args"},{"containerName":"BUILDARGS","line":44,"name":"$args","kind":13},{"line":46,"name":"%args","kind":13,"containerName":"BUILDARGS"}],"containerName":"Founder::Payment::Receipt","defintion":"sub","name":"BUILDARGS","kind":12},{"line":42,"name":"payment","kind":12},{"name":"id","line":43,"kind":12},{"name":"payment","line":43,"kind":12},{"kind":12,"line":43,"name":"id","containerName":"Founder::Payment::Receipt"},{"kind":12,"line":44,"name":"issuer_id"},{"line":44,"name":"payment","kind":12},{"containerName":"Founder::Payment::Receipt","line":44,"name":"issuer_id","kind":12},{"line":49,"children":[{"defintion":"my","kind":13,"name":"$self","line":50,"containerName":"request"},{"containerName":"request","line":52,"name":"$self","kind":13},{"containerName":"request","line":54,"name":"$payment","kind":13,"defintion":"my"},{"kind":13,"name":"$self","line":54,"containerName":"request"},{"containerName":"request","defintion":"my","name":"$item","line":55,"kind":13},{"containerName":"request","kind":13,"name":"$payment","line":58},{"containerName":"request","kind":13,"name":"$payment","line":59},{"kind":13,"name":"$payment","line":60,"containerName":"request"},{"name":"$tax_mode","line":63,"kind":13,"defintion":"my","containerName":"request"},{"name":"$type","line":64,"kind":13,"defintion":"my","containerName":"request"},{"containerName":"request","defintion":"my","line":65,"name":"$pay_method","kind":13},{"containerName":"request","name":"$resp","line":67,"kind":13,"defintion":"my"},{"containerName":"request","name":"$type","line":67,"kind":13},{"name":"$pay_method","line":67,"kind":13,"containerName":"request"},{"containerName":"request","name":"$self","line":67,"kind":13},{"containerName":"request","line":67,"name":"$payment","kind":13},{"containerName":"request","line":67,"name":"$tax_mode","kind":13},{"kind":13,"name":"$item","line":67,"containerName":"request"},{"containerName":"request","line":68,"name":"$payment","kind":13},{"containerName":"request","line":70,"name":"$resp","kind":13},{"kind":13,"name":"$resp","line":70,"containerName":"request"},{"containerName":"request","line":71,"name":"$self","kind":13},{"containerName":"request","line":72,"name":"$self","kind":13},{"containerName":"request","kind":13,"line":72,"name":"$resp"},{"containerName":"request","kind":13,"name":"$self","line":73},{"kind":13,"name":"$self","line":74,"containerName":"request"},{"kind":13,"name":"$resp","line":77,"containerName":"request"},{"containerName":"request","line":78,"name":"$self","kind":13},{"containerName":"request","kind":13,"name":"$self","line":80},{"kind":13,"line":80,"name":"$resp","containerName":"request"},{"line":82,"name":"$self","kind":13,"containerName":"request"},{"containerName":"request","line":83,"name":"$self","kind":13},{"containerName":"request","name":"$self","line":84,"kind":13},{"name":"$self","line":85,"kind":13,"containerName":"request"}],"name":"request","kind":12,"defintion":"sub","containerName":"Founder::Payment::Receipt"},{"kind":12,"name":"status","line":52,"containerName":"Founder::Payment::Receipt"},{"containerName":"Founder::Payment::Receipt","kind":12,"name":"payment","line":54},{"line":56,"name":"tax_type","kind":12},{"containerName":"Config","kind":12,"name":"Founder","line":56},{"containerName":"Founder::Payment::Receipt","line":56,"name":"get","kind":12},{"name":"Founder","line":56,"kind":12,"containerName":"Const::FISCAL_CONFIG_PATH"},{"kind":12,"line":57,"name":"quantity"},{"line":58,"name":"description","kind":12},{"line":58,"name":"description","kind":12,"containerName":"Founder::Payment::Receipt"},{"name":"price","line":59,"kind":12},{"kind":12,"line":59,"name":"amount","containerName":"Founder::Payment::Receipt"},{"line":60,"name":"type","kind":12},{"line":60,"name":"is_refund","kind":12,"containerName":"Founder::Payment::Receipt"},{"containerName":"Config","kind":12,"name":"Founder","line":63},{"line":63,"name":"get","kind":12,"containerName":"Founder::Payment::Receipt"},{"containerName":"Const::FISCAL_CONFIG_PATH","kind":12,"name":"Founder","line":63},{"containerName":"Fiscal::Receipt","kind":12,"name":"DMR","line":67},{"line":67,"name":"create","kind":12,"containerName":"Founder::Payment::Receipt"},{"containerName":"Founder::Payment::Receipt","kind":12,"line":67,"name":"issuer_id"},{"line":67,"name":"amount","kind":12,"containerName":"Founder::Payment::Receipt"},{"kind":12,"name":"user_email","line":68},{"containerName":"Founder::Payment::Receipt","line":68,"name":"user","kind":12},{"kind":12,"line":68,"name":"email","containerName":"Founder::Payment::Receipt"},{"name":"receipt_id","line":70,"kind":12},{"line":71,"name":"log_info","kind":12},{"containerName":"Founder::Payment::Receipt","line":72,"name":"receipt_id","kind":12},{"kind":12,"line":72,"name":"receipt_id"},{"kind":12,"name":"status","line":73,"containerName":"Founder::Payment::Receipt"},{"kind":12,"name":"update","line":74,"containerName":"Founder::Payment::Receipt"},{"name":"log_error","line":78,"kind":12},{"kind":12,"name":"log_error","line":80},{"name":"create_at","line":82,"kind":12,"containerName":"Founder::Payment::Receipt"},{"name":"FATAL_TIMEOUT","line":82,"kind":12},{"name":"log_fatal","line":83,"kind":12},{"containerName":"Founder::Payment::Receipt","kind":12,"line":84,"name":"status"},{"kind":12,"name":"update","line":85,"containerName":"Founder::Payment::Receipt"},{"children":[{"containerName":"check","defintion":"my","kind":13,"line":93,"name":"$self"},{"kind":13,"name":"$self","line":94,"containerName":"check"},{"containerName":"check","line":94,"name":"$self","kind":13},{"containerName":"check","defintion":"my","kind":13,"line":96,"name":"$resp"},{"containerName":"check","kind":13,"line":96,"name":"$self"},{"kind":13,"name":"$self","line":97,"containerName":"check"},{"name":"$resp","line":98,"kind":13,"containerName":"check"},{"kind":13,"line":99,"name":"$self","containerName":"check"},{"kind":13,"name":"$self","line":100,"containerName":"check"},{"containerName":"check","line":103,"name":"$resp","kind":13},{"containerName":"check","kind":13,"line":104,"name":"$self"},{"containerName":"check","kind":13,"name":"$resp","line":104},{"containerName":"check","name":"$self","line":105,"kind":13},{"containerName":"check","line":108,"name":"$resp","kind":13},{"kind":13,"line":109,"name":"$self","containerName":"check"},{"name":"$self","line":110,"kind":13,"containerName":"check"},{"containerName":"check","kind":13,"line":111,"name":"$resp"},{"kind":13,"line":112,"name":"$self","containerName":"check"},{"name":"$resp","line":112,"kind":13,"containerName":"check"},{"name":"$self","line":113,"kind":13,"containerName":"check"},{"line":115,"name":"$self","kind":13,"containerName":"check"},{"kind":13,"line":115,"name":"$resp","containerName":"check"},{"line":117,"name":"$self","kind":13,"containerName":"check"}],"line":92,"containerName":"Founder::Payment::Receipt","kind":12,"name":"check","defintion":"sub"},{"containerName":"Founder::Payment::Receipt","kind":12,"line":94,"name":"status"},{"name":"status","line":94,"kind":12,"containerName":"Founder::Payment::Receipt"},{"containerName":"Fiscal::Receipt","name":"DMR","line":96,"kind":12},{"containerName":"Founder::Payment::Receipt","name":"status","line":96,"kind":12},{"kind":12,"line":96,"name":"receipt_id","containerName":"Founder::Payment::Receipt"},{"line":97,"name":"last_check_time","kind":12,"containerName":"Founder::Payment::Receipt"},{"name":"log_error","line":99,"kind":12},{"name":"update","line":100,"kind":12,"containerName":"Founder::Payment::Receipt"},{"name":"error","line":103,"kind":12},{"name":"log_error","line":104,"kind":12},{"kind":12,"name":"update","line":105,"containerName":"Founder::Payment::Receipt"},{"kind":12,"line":108,"name":"status"},{"kind":12,"line":109,"name":"log_info"},{"kind":12,"name":"status","line":110,"containerName":"Founder::Payment::Receipt"},{"line":111,"name":"status","kind":12},{"kind":12,"line":112,"name":"log_fatal"},{"kind":12,"line":113,"name":"status","containerName":"Founder::Payment::Receipt"},{"kind":12,"name":"log_warn","line":115},{"kind":12,"name":"update","line":117,"containerName":"Founder::Payment::Receipt"},{"containerName":"ActiveRecordPg","kind":12,"name":"MR","line":121},{"containerName":"Founder::Payment::Receipt","kind":12,"line":122,"name":"meta"},{"name":"make_immutable","line":122,"kind":12,"containerName":"Founder::Payment::Receipt"}]