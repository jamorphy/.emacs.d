(setq org-html-preamble t)
(setq org-html-preamble
      (concat
       "<div style='display: flex; justify-content: center'>"
       "<ul id='banner'>"
       "<li> <a href='../../index.html'><< back</a></li>"
       "</ul>"
       "</div>"
       "<style>"
      "#banner { margin:0; list-style-type: none }"
      "#banner li {"
      "  float: left;"
      "}"
      "#banner li a, #banner li a:visited {"
        "display: block;"
        "padding: 10px;"
        "color: #07a;"
        "text-decoration: none;"
      "}"
      "</style><br>"))

(setq org-html-postamble t
      org-html-postamble-format '(("en" "<p class='date'>Last updated: %C</p>"))
      org-html-head-include-scripts nil
      org-html-head-include-default-style nil
      org-html-html5-fancy t
      org-html-doctype "html5"
      )
