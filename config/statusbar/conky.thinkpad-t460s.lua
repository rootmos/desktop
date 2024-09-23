local template = [[
^fg(\#aaaaaa)↓${downspeedf IFACE}^fg(\#FFFFFF) ^fg(\#aaaaaa)↑${upspeedf IFACE}^fg(\#FFFFFF) \
| ^fg(\#aaaaaa)${exec countdown}^fg(\#FFFFFF) \
| ^fg(\#aaaaaa)${exec monitor-client ping avg}ms ${exec monitor-client ping loss}% ${exec monitor-client location}^fg(\#FFFFFF) \
| ^fg(\#aaaaaa)/ ${exec monitor-client fs available /}^fg(\#FFFFFF) \
^fg(\#aaaaaa)/tmp ${exec monitor-client fs usage /tmp}%^fg(\#FFFFFF) \
^fg(\#aaaaaa)/home ${exec monitor-client fs available /home}^fg(\#FFFFFF) \
| ^fg(\#aaaaaa)C:${cpu}% T:${hwmon MON temp TEMP}° S:${swapperc}% M:${memperc}%^fg(\#FFFFFF) \
| ^fg(\#aaaaaa)${battery_short BAT} (${battery_time BAT})^fg(\#FFFFFF) \
| ^fg(\#aaaaaa)${time %Y-%m-%d} ^fg(\#ebac54)${time %R}
]]

template = template:gsub("IFACE", "wlp4s0")
template = template:gsub("BAT", "BAT0")
template = template:gsub("MON", 1):gsub("TEMP", 1)

conky.text = template
