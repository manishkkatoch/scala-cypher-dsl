 #!/usr/bin/env sh

 openssl aes-256-cbc -K $encrypted_54b5438000ae_key -iv $encrypted_54b5438000ae_iv -in $ENCRYPTED_GPG_KEY_LOCATION -out $GPG_KEY_LOCATION -d