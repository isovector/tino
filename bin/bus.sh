curl -s 'https://nb.translink.ca/nextbus.ashx?cp=gssr%2FdDFYR7%2FWGgqZygd89YZSwA==;003' \
  -H 'accept: application/json, text/javascript, */*; q=0.01' \
  -H 'accept-language: en-GB,en;q=0.8' \
  -H 'cache-control: max-age=0' \
  -H 'cookie: ASP.NET_SessionId=dwyjxbgvr3wdtzs33maxjrau; nb.cookie=enabled; nb.cookie.test=null' \
  -H 'referer: https://nb.translink.ca/text/stop/59837/route/003' \
  -H 'x-requested-with: XMLHttpRequest' \
  --compressed | jq -r '.NextBuses[0].Schedules | .[] | select(.CancelledStop == false and .CancelledTrip == false) | [(.ExpectedLeaveTime | split(" "))[0], .ExpectedCountdown | tostring] | @tsv'
