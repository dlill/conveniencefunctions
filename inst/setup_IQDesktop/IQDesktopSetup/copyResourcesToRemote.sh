ssh-copy-id -p 6913 daniel2@192.168.1.11
scp -P 6913 id_rsa.zip daniel2@192.168.1.11:~/id_rsa.zip
scp -P 6913 Resources/setupRemote.sh daniel2@192.168.1.11:~/setupRemote.sh