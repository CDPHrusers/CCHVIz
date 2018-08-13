
  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());

  gtag('config', 'UA-8317364-4');

  
$(document).on('shiny:inputchanged', function(event) {
       if (event.name == 'exposure' || event.name == 'sensitivity') {
          gtag('event', event.name, {
			  'event_action': 'Select Input',
			  'event_category': 'Vulnerability',
			  'event_label': event.value
		  });
	  
       }
       if (event.name == 'cnty1') {
          gtag('event', event.name, {
			  'event_action': 'Select Input',
			  'event_category': 'County Snapshot',
			  'event_label': event.value
		  });
       }
	  if (event.name == 'cnty' || event.name == 'ind' || event.name == 'strt') {
          gtag('event', event.name, {
			  'event_action': 'Select Input',
			  'event_category': 'Single Indicator',
			  'event_label': event.value
		  });
       }
	  if (event.name == 'cntyDNLD' || event.name == 'indDNLD' ) {
          gtag('event', event.name, {
			  'event_action': 'Select Input',
			  'event_category': 'Query the Data',
			  'event_label': event.value
		  });
       }
     });
	 