export SODIUM_LIB_DIR=/nix/store/y95hjmcslzmlziv3zllqq3j654gqpqnx-libsodium-1.0.18-dev/lib/pkgconfig/
export PREFIX_INSTALL=`pwd`/libzauth
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$PREFIX_INSTALL/lib/
export EXTRA_CC_LIB="-L$PREFIX_INSTALL/lib/ -lzauth"
export EXTRA_CC_INC="-I$PREFIX_INSTALL/include"
