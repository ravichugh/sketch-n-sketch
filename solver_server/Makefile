test_reduce: ensure_reduce
	cd reduce-algebra && scripts/testall.sh --csl

reduce-algebra:
	rm -rf reduce-algebra
	svn co http://svn.code.sf.net/p/reduce-algebra/code/trunk reduce-algebra

reduce-algebra/bin/redcsl: reduce-algebra
	# May need to run sudo apt-get install libtool-bin
	rm reduce-algebra/bin/redcsl; cd reduce-algebra && ./configure --with-csl --without-gui && make

ensure_reduce:
	(echo "" | reduce-algebra/bin/redcsl > /dev/null) || make reduce-algebra/bin/redcsl

run_server: ensure_reduce
	# Requires websocketd (e.g. $ brew install websocketd) and Ruby
	# This will open a websocket server at localhost:7444/reduce
	./sketch-n-sketch_solver_server


### For playing with REDUCE ###

reduce_repl: ensure_reduce
	reduce-algebra/bin/redcsl

examples: ensure_reduce
	# See Section 7.10 in the REDUCE manual http://www.reduce-algebra.com/reduce38-docs/reduce.pdf#section.7.10
	cat examples.txt | tee /dev/tty | reduce-algebra/bin/redcsl


### Debian Only ###

setup:
	sudo apt-get install ncurses-dev
	svn --version || sudo apt-get install subversion
	libtoolize --version || sudo apt-get install libtool-bin
	(echo "" | reduce-algebra/bin/redcsl) || make test_reduce
	ruby --version || sudo apt-get install ruby
	which websocketd || (wget https://github.com/joewalnes/websocketd/releases/download/v0.2.12/websocketd-0.2.12-linux_amd64.zip && unzip websocketd-0.2.12-linux_amd64.zip && sudo mv websocketd /usr/local/bin/)
	make daemonize status

# https://certbot.eff.org/lets-encrypt/ubuntubionic-other
/etc/letsencrypt/live/tormenta.cs.uchicago.edu/fullchain.pem:
	sudo apt-get update
	sudo apt-get install software-properties-common
	sudo add-apt-repository universe
	sudo add-apt-repository ppa:certbot/certbot
	sudo apt-get update
	sudo apt-get install certbot
	sudo certbot certonly --standalone
	sudo certbot renew --dry-run

daemonize: /etc/letsencrypt/live/tormenta.cs.uchicago.edu/fullchain.pem
	sudo systemctl stop sketch-n-sketch_solver_server; sudo systemctl disable sketch-n-sketch_solver_server; sudo rm /etc/systemd/system/sketch-n-sketch_solver_server.service; sudo cp sketch-n-sketch_solver_server.service /etc/systemd/system/
	sudo systemctl enable sketch-n-sketch_solver_server
	sudo systemctl start sketch-n-sketch_solver_server

status:
	systemctl status sketch-n-sketch_solver_server

log:
	sudo journalctl -u sketch-n-sketch_solver_server

tail_log:
	sudo journalctl -f -u sketch-n-sketch_solver_server


### Run from a different computer ###

remake_public_server:
	ssh -t brian@tormenta.cs.uchicago.edu "cd sketch-n-sketch && git pull --rebase && cd solver_server && (rm -r reduce-algebra; make setup)"

refresh_public_server:
	ssh -t brian@tormenta.cs.uchicago.edu "cd sketch-n-sketch && git pull --rebase && cd solver_server && make daemonize status"
