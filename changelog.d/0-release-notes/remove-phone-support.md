Phone registration and login is not supported anymore. All API endpoints dealing with phone numbers and phone activation codes now fail with a 400 error. Brig options related to phone number support have now been deleted, namely:
 - `setTwilio`
 - `setNexmo`
 - `setAllowlistPhonePrefixes`.
