local template = [[
^fg(\#aaaaaa)↓${downspeedf IFACE}^fg(\#FFFFFF) ^fg(\#aaaaaa)↑${upspeedf IFACE}^fg(\#FFFFFF) \
| ^fg(\#aaaaaa)${exec env BAR_WIDTH=30 countdown show}^fg(\#FFFFFF) \
| ^fg(\#aaaaaa)${exec monitor-client ping avg}ms ${exec monitor-client ping loss}% ${exec monitor-client location}^fg(\#FFFFFF) \
| ^fg(\#aaaaaa)/ ${exec monitor-client fs available /}^fg(\#FFFFFF) \
^fg(\#aaaaaa)/tmp ${exec monitor-client fs usage /tmp}%^fg(\#FFFFFF) \
^fg(\#aaaaaa)/home ${exec monitor-client fs available /home}^fg(\#FFFFFF) \
| ^fg(\#aaaaaa)C:${cpu}% T:${hwmon MON temp TEMP}° S:${swapperc}% M:${memperc}%^fg(\#FFFFFF) \
| ^fg(\#aaaaaa)${battery_short BAT0} (${battery_time BAT0})^fg(\#FFFFFF) ^fg(\#aaaaaa)${battery_short BAT1} (${battery_time BAT1})^fg(\#FFFFFF) \
| ^fg(\#aaaaaa)${time %Y-%m-%d} ^fg(\#ebac54)${time %R}
]]

template = template:gsub("IFACE", "wlp4s0")
template = template:gsub("MON", 1):gsub("TEMP", 1)

conky.text = template
