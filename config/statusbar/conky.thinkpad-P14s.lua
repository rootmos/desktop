local template = [[
^fg(\#aaaaaa)↓${downspeedf IFACE}^fg(\#FFFFFF) ^fg(\#aaaaaa)↑${upspeedf IFACE}^fg(\#FFFFFF) \
| ^fg(\#aaaaaa)${exec monitor-client ping avg}ms ${exec monitor-client ping loss}% ${exec monitor-client location}^fg(\#FFFFFF) \
| ^fg(\#aaaaaa)/ ${exec monitor-client fs available /}^fg(\#FFFFFF) \
^fg(\#aaaaaa)/home ${exec monitor-client fs available /home}^fg(\#FFFFFF) \
| ^fg(\#aaaaaa)C:${cpu}% T:${acpitemp}° F:${ibm_fan} S:${swapperc}% M:${memperc}%^fg(\#FFFFFF) \
| ^fg(\#aaaaaa)${battery_short BAT0} (${battery_time BAT0})^fg(\#FFFFFF) \
| ^fg(\#aaaaaa)${time %Y-%m-%d} ^fg(\#ebac54)${time %R}
]]

template = template:gsub("IFACE", "wlan0")

conky.text = template
