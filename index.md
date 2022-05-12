
I embarked on an effort to make Connecticut hospitals' price transparency files — required by the Centers for Medicare & Medicaid Services beginning in Jan. 2021 — readable and comparable. We specifically focused on the negotiated rates that hospitals have with various health insurance plans. The data sources are scattered across every hospital or health system's website, and can be hard to track down.

The resulting dataset, which totaled about 600,000 rows of information for just one set of billing codes hospitals were required to share information about, is full of interesting insights. <a href="https://www.ctinsider.com/projects/2022/hospital-care-costs-connecticut/?src=ctipdensecp">Read the story in CT Insider here.</a>

But ultimately, I found it difficult to execute a higher-level data analysis with these records. Too many questions arose: Would this constitute an ethical sample for drawing conclusions? Did hospitals really post all of their information, and would we be punishing the best reporters with this kind of analysis? 

But if you're interested in just one service, it's possible to see some interseting takeaways. Take for example the negotiated rates for two different kinds of births across just one health system, the largest in Connecticut:

<iframe title="Negotiated rates for a traditional delivery at Hartford Healthcare" aria-label="Dot Plot" id="datawrapper-chart-gBpNg" src="https://datawrapper.dwcdn.net/gBpNg/1/" scrolling="no" frameborder="0" style="width: 0; min-width: 100% !important; border: none;" height="266"></iframe><script type="text/javascript">!function(){"use strict";window.addEventListener("message",(function(e){if(void 0!==e.data["datawrapper-height"]){var t=document.querySelectorAll("iframe");for(var a in e.data["datawrapper-height"])for(var r=0;r<t.length;r++){if(t[r].contentWindow===e.source)t[r].style.height=e.data["datawrapper-height"][a]+"px"}}}))}();
</script>
<iframe title="Negotiated rates for a C-section at Hartford Healthcare" aria-label="Dot Plot" id="datawrapper-chart-daKcI" src="https://datawrapper.dwcdn.net/daKcI/1/" scrolling="no" frameborder="0" style="width: 0; min-width: 100% !important; border: none;" height="221"></iframe><script type="text/javascript">!function(){"use strict";window.addEventListener("message",(function(e){if(void 0!==e.data["datawrapper-height"]){var t=document.querySelectorAll("iframe");for(var a in e.data["datawrapper-height"])for(var r=0;r<t.length;r++){if(t[r].contentWindow===e.source)t[r].style.height=e.data["datawrapper-height"][a]+"px"}}}))}();
</script>

I am excited for the release of this same data, but reported by insurance companies, beginning in July. The benefit of having that information is we can be sure to have standardized datasets that will represent ranges in hospital prices by insurance company, rather than the other way around. Right now, hospitals' data showing insurance plan certainly meets the definition of "dirty data." 
  
<a href="https://github.com/marykwild/pricetransparency">Head to the main repository</a> to better understand the scope of the project and where we sourced the information. 
