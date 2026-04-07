#!/usr/bin/env python3
from capstone import *
from capstone.x86 import *
from elftools.elf.elffile import ELFFile
import sys

# --- Configure your policy here ---
# Example policy: allow up to SSE4.2, but forbid AVX/AVX2/AVX512 and also forbid BMI/ADX.
FORBIDDEN_GROUPS = {
    #    "AVX": X86_GRP_AVX,
    #    "AVX2": X86_GRP_AVX2,
    "AVX512": X86_GRP_AVX512,
    # You can add more depending on what you consider "too new".
    # Note: Capstone's group coverage is good for SIMD families, less so for every x86 feature.
}

# Optional: also flag specific mnemonics (useful for BMI1/BMI2 etc. if group coverage isn’t enough)
FORBIDDEN_MNEMONICS = {
    # "tzcnt", "lzcnt", "andn", "bextr", "pdep", "pext", "mulx", "adox", "adcx",
}

def iter_exec_sections(elffile):
    for sec in elffile.iter_sections():
        sh = sec.header
        # SHF_EXECINSTR = 0x4
        if (sh["sh_flags"] & 0x4) and sh["sh_size"] > 0:
            yield sec

def main(path):
    with open(path, "rb") as f:
        elf = ELFFile(f)

        # Capstone x86-64
        md = Cs(CS_ARCH_X86, CS_MODE_64)
        md.detail = True
        md.skipdata = True

        any_bad = False

        for sec in iter_exec_sections(elf):
            code = sec.data()
            addr = sec["sh_addr"]

            for insn in md.disasm(code, addr):
                # Skip SKIPDATA pseudo-instructions (data, not code)
                if insn.id == 0:
                    continue

                bad_reasons = []

                # mnemonic-based checks
                if insn.mnemonic in FORBIDDEN_MNEMONICS:
                    bad_reasons.append(f"mnemonic:{insn.mnemonic}")

                # group-based checks
                for name, grp in FORBIDDEN_GROUPS.items():
                    if grp in insn.groups:
                        bad_reasons.append(f"group:{name}")

                if bad_reasons:
                    any_bad = True
                    # print minimal but actionable info
                    print(f"{path}:{sec.name}:0x{insn.address:x}:  {insn.mnemonic} {insn.op_str}   ({', '.join(bad_reasons)})")

        sys.exit(1 if any_bad else 0)

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(f"usage: {sys.argv[0]} /path/to/binary", file=sys.stderr)
        sys.exit(2)
    main(sys.argv[1])
