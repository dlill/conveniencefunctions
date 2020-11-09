ssh-copy-id -p 6913 daniel2@192.168.1.12
scp -P 6913 /c/PROJTOOLS/IQDesktop/id_rsa.zip  daniel2@192.168.1.12:~/id_rsa.zip
scp -P 6913 /c/PROJTOOLS/conveniencefunctions/inst/setup_IQDesktop/Setup/Resources/setupRemote.sh daniel2@192.168.1.12:~/setupRemote.sh
ssh -p 6913 daniel2@192.168.1.12 'cd && chmod +x setupRemote.sh'

# /IQDESKTOP/PROJTOOLS/conveniencefunctions/inst/setup_IQDesktop/Setup/Resources/setupLocal.sh