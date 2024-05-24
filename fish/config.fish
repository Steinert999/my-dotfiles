function neofetch
	fastfetch
end
function fish_greeting
	fastfetch	
end
if status --is-login
	set -gx PATH $PATH ~/bin set -gx PATH $PATH ~/usr/local/bin
	set -gx PATH $PATH ~/.local/bin
end 
if status is-interactive
	source ~/.asdf/asdf.fish
	source ~/.asdf/plugins/java/set-java-home.fish
	set -gx EDITOR code

	if test -f "~/.ghcup/env"
		source ~/.ghcup/env
	end
	alias ls "exa --icons"
	alias cat "bat"
	function pacman-clear
		pacman -Qtdq | sudo pacman -Rs -  $argv
	end
	function xmonad-config
		$EDITOR ~/.xmonad
	end
	function fish-config
		$EDITOR ~/.config/fish/config.fish $argv
	end
	function oh-my-fish
		$EDITOR ~/.config/omf $argv
	end
	function download-song
		yt-dlp -x -f bestaudio --audio-format mp3 -P ~/my-musics $argv
	end
	function download-song-playlist
		download-song --ignore-errors --continue --no-overwrites --download-archive progress.txt $argv
	end
	function download-video
		yt-dlp -S vcoded:h264,red,acodec:m4a -P ~/my-videos $argv
	end
	function download-video-playlist
		download-video --ignore-errors --continue --no-overwrites --download-archive progress.txt $argv
	end
	starship init fish | source
	zoxide init --cmd cd fish | source
end
