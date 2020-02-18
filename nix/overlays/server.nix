self: super: {
  wire.digitalOceanImage = super.nixos ({modulesPath,...}:{
    imports = [
      (modulesPath + "/virtualisation/digital-ocean-image.nix")
      ./in-a-box.nix
    ];
  }).digitalOceanImage;

  wire.container = super.nixos({modulesPath,...}:{
    imports = [
      (modulesPath + "/virtualisation/container-config.nix")
      ./in-a-box.nix
    ];
    config.boot.isContainer = true;
  });
  wire.local-vm = super.nixos ({modulesPath,...}:{
    imports = [
      (modulesPath + "/virtualisation/qemu-vm.nix")
      ./in-a-box.nix
    ];
  });
}
