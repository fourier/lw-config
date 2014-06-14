on run
	tell application "LispWorks Personal"
		reopen
		activate
	end tell
	
	tell application "System Events"
		tell process "lispworks-personal-6-1-1-macos-universal"
			set frontmost to true
			if exists window "LispWorks Personal Edition" then
				tell window "LispWorks Personal Edition"
					tell button "Close"
						click
					end tell
				end tell
			end if
			delay 0.1
			set frontmost to true
			tell window "Listener 1"
				keystroke "(load \"~/.lispworks\")"
				keystroke return
			end tell
			set frontmost to true
		end tell
	end tell
	
	--tell application "LispWorks Personal"
	--	activate
	--end tell
	
end run