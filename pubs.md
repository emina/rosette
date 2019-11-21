---
title: Papers
---

## Papers 

<ul class="bibliography">
  {% for pub in site.data.bib %}<li id="{{ pub.key }}">[{{ forloop.index0 | plus: 1}}] {{ pub.author }}. {{ pub.title }}. {{ pub.booktitle }}, {{ pub.year }}.
  {%- if pub.award %} <b>{{ pub.award }}</b>.{% endif -%}
  {%- if pub.url or pub.preprint or pub.web %} ({% if pub.url %}<a href="{{ pub.url }}">DOI</a>{% endif %}{% if pub.preprint %}{% if pub.url %}, {% endif %}<a href="{{ pub.preprint }}">PDF</a>{% endif %}{% if pub.web %}{% if pub.url or pub.preprint %}, {% endif %}<a href="{{ pub.web }}">Web</a>{% endif %}){% endif -%}</li>
  {% endfor %}
</ul>
