export SODIUM_LIB_DIR=/nix/store/y95hjmcslzmlziv3zllqq3j654gqpqnx-libsodium-1.0.18-dev/lib/pkgconfig/
export PREFIX_INSTALL=`pwd`/libzauth
export EXTRA_CC_LIB="-l$PREFIX_INSTALL/lib/libzauth.so"
export EXTRA_CC_INC="-I$PREFIX_INSTALL/include"
