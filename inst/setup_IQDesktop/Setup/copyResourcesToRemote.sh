docker exec -u daniel2 -d setup_IQdesktop_1 sh -c 'mkdir /home/daniel2/.ssh'
docker cp /home/daniel/.ssh/id_rsa setup_IQdesktop_1:/home/daniel2/.ssh/
docker cp /home/daniel/.ssh/id_rsa.pub setup_IQdesktop_1:/home/daniel2/.ssh/
docker cp /home/daniel/Promotion/Promotion/Projects/conveniencefunctions/inst/setup_IQDesktop/Setup/Resources/setupLocal.sh setup_IQdesktop_1:/home/daniel2/

docker exec -u daniel2 -it setup_IQdesktop_1 sh -c 'cd /home/daniel2/ && sudo chown daniel2 setupLocal.sh && chmod +x setupLocal.sh && ./setupLocal.sh'

# /IQDESKTOP/PROJTOOLS/conveniencefunctions/inst/setup_IQDesktop/Setup/Resources/setupLocal.sh
