self: super: {
  cryptobox = self.callPackage ./pkgs/cryptobox { };
  zauth = self.callPackage ./pkgs/zauth { };

  nginxModules = super.nginxModules // {
    zauth = {
      src = ../services/nginz/third_party/nginx-zauth-module;
      inputs = [ self.pkg-config self.zauth ];
    };
  };

  nginz = super.nginx.override {
    modules = [
      self.nginxModules.vts
      self.nginxModules.moreheaders
      self.nginxModules.zauth
    ];
  };
}
