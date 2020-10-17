[{"kind":2,"line":0,"name":"MyDawn::OAuth::Social::Twitch","defintion":1},{"containerName":"","name":"Mouse","line":2,"kind":2},{"name":"XS","containerName":"JSON","kind":2,"line":4},{"line":5,"kind":2,"containerName":"URI::Escape","name":"XS"},{"line":6,"kind":2,"containerName":"AnyEvent","name":"HTTP"},{"kind":2,"line":7,"name":"Coro","containerName":""},{"containerName":"MR","name":"Log","line":9,"kind":2},{"line":10,"kind":2,"containerName":"MyDawn","name":"Config"},{"containerName":"MyDawn::OAuth","name":"Config","line":11,"kind":2},{"line":12,"kind":2,"containerName":"MyDawn::OAuth","name":"Const"},{"name":"extends","kind":12,"line":14},{"name":"oauth_server","line":16,"kind":12},{"line":17,"kind":12,"name":"is"},{"line":18,"kind":12,"name":"isa"},{"name":"default","kind":12,"line":19},{"line":22,"kind":12,"name":"api_server"},{"line":23,"kind":12,"name":"is"},{"line":24,"kind":12,"name":"isa"},{"name":"default","kind":12,"line":25},{"line":28,"children":[],"name":"name","containerName":"MyDawn::OAuth::Social::Twitch","kind":12,"defintion":"sub"},{"name":"MyDawn","containerName":"OAuth::Const::SOURCE_TWITCH","kind":12,"line":28},{"name":"get_auth_info","containerName":"MyDawn::OAuth::Social::Twitch","children":[{"kind":13,"line":31,"name":"$self","containerName":"get_auth_info","defintion":"my"},{"kind":13,"line":31,"name":"%params","containerName":"get_auth_info"},{"defintion":"my","kind":13,"line":32,"name":"$code","containerName":"get_auth_info"},{"line":32,"kind":13,"containerName":"get_auth_info","name":"$params"},{"line":33,"kind":13,"containerName":"get_auth_info","name":"$client_id","defintion":"my"},{"line":33,"kind":13,"containerName":"get_auth_info","name":"$self"},{"containerName":"get_auth_info","name":"$client_secret","line":34,"kind":13,"defintion":"my"},{"line":34,"kind":13,"containerName":"get_auth_info","name":"$self"},{"line":35,"kind":13,"containerName":"get_auth_info","name":"$redirect_uri","defintion":"my"},{"name":"$params","containerName":"get_auth_info","kind":13,"line":35},{"line":35,"kind":13,"containerName":"get_auth_info","name":"$params"},{"name":"$self","containerName":"get_auth_info","kind":13,"line":35},{"containerName":"get_auth_info","name":"$auth_request_str","line":36,"kind":13,"defintion":"my"},{"line":36,"kind":13,"containerName":"get_auth_info","name":"$self"},{"name":"$body","containerName":"get_auth_info","kind":13,"line":37,"defintion":"my"},{"containerName":"get_auth_info","name":"$client_id","line":37,"kind":13},{"name":"$client_secret","containerName":"get_auth_info","kind":13,"line":38},{"name":"$code","containerName":"get_auth_info","kind":13,"line":40},{"line":41,"kind":13,"containerName":"get_auth_info","name":"$redirect_uri"},{"defintion":"my","line":43,"kind":13,"containerName":"get_auth_info","name":"$headers"},{"containerName":"get_auth_info","name":"$auth_info","line":45,"kind":13,"defintion":"my"},{"line":45,"kind":13,"containerName":"get_auth_info","name":"$self"},{"name":"$auth_request_str","containerName":"get_auth_info","kind":13,"line":47},{"kind":13,"line":48,"name":"$body","containerName":"get_auth_info"},{"kind":13,"line":49,"name":"$headers","containerName":"get_auth_info"},{"containerName":"get_auth_info","name":"$auth_info","line":52,"kind":13}],"line":30,"defintion":"sub","kind":12},{"line":32,"kind":12,"name":"code"},{"kind":12,"line":33,"name":"MyDawn","containerName":"Config"},{"line":33,"kind":12,"containerName":"MyDawn::OAuth::Social::Twitch","name":"get"},{"kind":12,"line":33,"name":"config","containerName":"MyDawn::OAuth::Social::Twitch"},{"line":34,"kind":12,"containerName":"Config","name":"MyDawn"},{"line":34,"kind":12,"containerName":"MyDawn::OAuth::Social::Twitch","name":"get"},{"line":34,"kind":12,"containerName":"MyDawn::OAuth::Social::Twitch","name":"config"},{"line":35,"kind":12,"containerName":"OAuth::Config::IS_TEST_SERVER","name":"MyDawn"},{"kind":12,"line":35,"name":"redirect_uri"},{"line":35,"kind":12,"name":"redirect_uri"},{"name":"uri_escape","kind":12,"line":35},{"containerName":"Config","name":"MyDawn","line":35,"kind":12},{"line":35,"kind":12,"containerName":"MyDawn::OAuth::Social::Twitch","name":"get"},{"kind":12,"line":35,"name":"config","containerName":"MyDawn::OAuth::Social::Twitch"},{"line":36,"kind":12,"containerName":"MyDawn::OAuth::Social::Twitch","name":"oauth_server"},{"kind":12,"line":40,"name":"uri_escape"},{"containerName":"MyDawn::OAuth::Social::Twitch","name":"request_json","line":45,"kind":12},{"name":"method","line":46,"kind":12},{"name":"uri","kind":12,"line":47},{"line":48,"kind":12,"name":"body"},{"line":49,"kind":12,"name":"headers"},{"defintion":"sub","kind":12,"children":[{"defintion":"my","kind":13,"line":56,"name":"$self","containerName":"get_profile_info"},{"name":"$access_token","containerName":"get_profile_info","kind":13,"line":56},{"defintion":"my","name":"$request_str","containerName":"get_profile_info","kind":13,"line":58},{"kind":13,"line":58,"name":"$self","containerName":"get_profile_info"},{"defintion":"my","line":59,"kind":13,"containerName":"get_profile_info","name":"$client_id"},{"line":59,"kind":13,"containerName":"get_profile_info","name":"$self"},{"name":"$access_token","containerName":"get_profile_info","kind":13,"line":60},{"containerName":"get_profile_info","name":"$req_headers","line":61,"kind":13,"defintion":"my"},{"line":61,"kind":13,"containerName":"get_profile_info","name":"$access_token"},{"line":61,"kind":13,"containerName":"get_profile_info","name":"$client_id"},{"name":"$cb","containerName":"get_profile_info","kind":13,"line":63,"defintion":"my"},{"line":64,"kind":13,"containerName":"get_profile_info","name":"$request_str"},{"kind":13,"line":64,"name":"$req_headers","containerName":"get_profile_info"},{"kind":13,"line":64,"name":"$cb","containerName":"get_profile_info"},{"defintion":"my","containerName":"get_profile_info","name":"$data","line":65,"kind":13},{"containerName":"get_profile_info","name":"$headers","line":65,"kind":13},{"name":"$cb","containerName":"get_profile_info","kind":13,"line":65},{"defintion":"my","line":67,"kind":13,"containerName":"get_profile_info","name":"$response_code"},{"name":"$headers","containerName":"get_profile_info","kind":13,"line":67},{"containerName":"get_profile_info","name":"$response_code","line":68,"kind":13},{"kind":13,"line":69,"name":"$data","containerName":"get_profile_info"},{"containerName":"get_profile_info","name":"$headers","line":69,"kind":13},{"name":"$response","containerName":"get_profile_info","kind":13,"line":73,"defintion":"my"},{"name":"$data","containerName":"get_profile_info","kind":13,"line":74},{"line":77,"kind":13,"containerName":"get_profile_info","name":"$data"},{"line":77,"kind":13,"containerName":"get_profile_info","name":"$headers"},{"kind":13,"line":80,"name":"$response","containerName":"get_profile_info"}],"line":55,"name":"get_profile_info","containerName":"MyDawn::OAuth::Social::Twitch"},{"name":"api_server","containerName":"MyDawn::OAuth::Social::Twitch","kind":12,"line":58},{"kind":12,"line":59,"name":"MyDawn","containerName":"Config"},{"line":59,"kind":12,"containerName":"MyDawn::OAuth::Social::Twitch","name":"get"},{"containerName":"MyDawn::OAuth::Social::Twitch","name":"config","line":59,"kind":12},{"containerName":"rouse_cb","name":"Coro","line":63,"kind":12},{"name":"http_get","line":64,"kind":12},{"name":"headers","line":64,"kind":12},{"name":"Coro","containerName":"rouse_wait","kind":12,"line":65},{"line":67,"kind":12,"name":"Status"},{"line":69,"kind":12,"name":"log_error"},{"kind":12,"line":70,"name":"error"},{"name":"error_description","line":70,"kind":12},{"kind":12,"line":74,"name":"JSON","containerName":"XS::decode_json"},{"name":"log_error","line":77,"kind":12},{"name":"error","line":78,"kind":12},{"kind":12,"line":78,"name":"error_description"},{"kind":12,"defintion":"sub","containerName":"MyDawn::OAuth::Social::Twitch","name":"get_bio","children":[{"defintion":"my","containerName":"get_bio","name":"$self","line":84,"kind":13},{"containerName":"get_bio","name":"$result","line":85,"kind":13,"defintion":"my"},{"line":85,"kind":13,"containerName":"get_bio","name":"$self"},{"containerName":"get_bio","name":"$result","line":87,"kind":13},{"line":87,"kind":13,"containerName":"get_bio","name":"$result"},{"line":88,"kind":13,"containerName":"get_bio","name":"$result"},{"containerName":"get_bio","name":"$result","line":88,"kind":13},{"kind":13,"line":89,"name":"$result","containerName":"get_bio"},{"line":89,"kind":13,"containerName":"get_bio","name":"$result"},{"line":90,"kind":13,"containerName":"get_bio","name":"$result"},{"name":"$result","containerName":"get_bio","kind":13,"line":90},{"containerName":"get_bio","name":"$result","line":91,"kind":13},{"containerName":"get_bio","name":"$result","line":91,"kind":13},{"containerName":"get_bio","name":"$result","line":92,"kind":13},{"name":"$result","containerName":"get_bio","kind":13,"line":92},{"containerName":"get_bio","name":"$result","line":93,"kind":13}],"line":83},{"containerName":"MyDawn::OAuth::Social::Twitch","name":"get_profile_info","line":85,"kind":12},{"kind":12,"line":87,"name":"error"},{"kind":12,"line":88,"name":"data"},{"line":89,"kind":12,"name":"avatar_url"},{"name":"profile_image_url","kind":12,"line":89},{"name":"uid","line":90,"kind":12},{"name":"id","kind":12,"line":90},{"name":"name","kind":12,"line":91},{"kind":12,"line":91,"name":"display_name"},{"name":"username","kind":12,"line":92},{"name":"login","kind":12,"line":92},{"line":96,"kind":12,"name":"Mouse"},{"containerName":"MyDawn::OAuth::Social::Twitch","name":"meta","line":97,"kind":12},{"name":"make_immutable","containerName":"MyDawn::OAuth::Social::Twitch","kind":12,"line":97}]