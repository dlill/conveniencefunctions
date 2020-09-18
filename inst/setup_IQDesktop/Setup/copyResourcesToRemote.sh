ssh-copy-id -p 6913 daniel2@192.168.1.11
scp -P 6913 /c/PROJTOOLS/IQDesktop/id_rsa.zip  daniel2@192.168.1.11:~/id_rsa.zip
scp -P 6913 /c/PROJTOOLS/conveniencefunctions/inst/setup_IQDesktop/Setup/Resources/setupRemote.sh daniel2@192.168.1.11:~/setupRemote.sh
ssh -p 6913 daniel2@192.168.1.11 'cd && chmod +x setupRemote.sh'
