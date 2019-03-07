#!/bin/bash
# Change dynamicaly the theme of
# the xfce4 terminal depending of
# the moment of the day.

THEME_DIR="${HOME}/.local/share/xfce4/terminal/colorschemes"
TERMINAL_CONFIG_FILE="${HOME}/.config/xfce4/terminal/terminalrc"

THEME_DARK_FILE="base16-gruvbox-dark-hard.16.theme"
THEME_LIGHT_FILE="base16-gruvbox-light-hard.16.theme"
CURRENT_THEME="${THEME_DIR}/${THEME_DARK_FILE}"

HOUR=$(date | cut -d',' -f 2 | cut -d':' -f 1 | tr -d '[:blank:]')
if [ ${HOUR} -lt 12 ]; then
  CURRENT_THEME="${THEME_DIR}/${THEME_LIGHT_FILE}"
fi

FILE_CONTENT_HIDDEN_OPTIONS="
[Configuration]
MiscAlwaysShowTabs=FALSE
MiscBell=FALSE
MiscBellUrgent=FALSE
MiscBordersDefault=TRUE
MiscCursorBlinks=FALSE
MiscCursorShape=TERMINAL_CURSOR_SHAPE_IBEAM
MiscDefaultGeometry=80x24
MiscInheritGeometry=FALSE
MiscMenubarDefault=TRUE
MiscMouseAutohide=FALSE
MiscMouseWheelZoom=TRUE
MiscToolbarDefault=FALSE
MiscConfirmClose=TRUE
MiscCycleTabs=TRUE
MiscTabCloseButtons=TRUE
MiscTabCloseMiddleClick=TRUE
MiscTabPosition=GTK_POS_TOP
MiscHighlightUrls=TRUE
MiscMiddleClickOpensUri=FALSE
MiscCopyOnSelect=FALSE
MiscShowRelaunchDialog=TRUE
MiscRewrapOnResize=TRUE
MiscUseShiftArrowsToScroll=FALSE
MiscSlimTabs=FALSE
MiscNewTabAdjacent=FALSE
TitleInitial=Terminal, tous le monde descend !
FontName=Hack 12
"

CONTENT_HIDDEN=$(echo "${FILE_CONTENT_HIDDEN_OPTIONS}")
CONTENT_COLOR=$(cat "${CURRENT_THEME}" | grep -Ev '^(\[Scheme\]|Name=)')
printf "%s\n%s" "${CONTENT_HIDDEN}" "${CONTENT_COLOR}" > "${TERMINAL_CONFIG_FILE}"
