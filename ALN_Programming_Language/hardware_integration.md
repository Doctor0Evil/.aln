# ALN Hardware Integration Guide v8.3.0

## Features:
- Native support for SLB9665 TPM, ATECC608A Crypto, STM32H7 MCU, Dimensity1000C SoC.
- Commands: @TPM_SECURE_BOOT, @CRYPTO_KEY_STORAGE

## Example:
@HARDWARE { chip: 'ATECC608A', action: 'key_derivation' }
