(function (global, factory) {
    typeof exports === 'object' && typeof module !== 'undefined' ? factory(exports) :
    typeof define === 'function' && define.amd ? define(['exports'], factory) :
    (factory((global.fc = global.fc || {})));
  }(this, (function (exports) { 'use strict';
  
  var createReboundMethod = (function (target, source, name) {
      var method = source[name];
      if (typeof method !== 'function') {
          throw new Error('Attempt to rebind ' + name + ' which isn\'t a function on the source object');
      }
      return function () {
          for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
              args[_key] = arguments[_key];
          }
  
          var value = method.apply(source, args);
          return value === source ? target : value;
      };
  });
  
  var rebind = (function (target, source) {
      for (var _len = arguments.length, names = Array(_len > 2 ? _len - 2 : 0), _key = 2; _key < _len; _key++) {
          names[_key - 2] = arguments[_key];
      }
  
      var _iteratorNormalCompletion = true;
      var _didIteratorError = false;
      var _iteratorError = undefined;
  
      try {
          for (var _iterator = names[Symbol.iterator](), _step; !(_iteratorNormalCompletion = (_step = _iterator.next()).done); _iteratorNormalCompletion = true) {
              var name = _step.value;
  
              target[name] = createReboundMethod(target, source, name);
          }
      } catch (err) {
          _didIteratorError = true;
          _iteratorError = err;
      } finally {
          try {
              if (!_iteratorNormalCompletion && _iterator.return) {
                  _iterator.return();
              }
          } finally {
              if (_didIteratorError) {
                  throw _iteratorError;
              }
          }
      }
  
      return target;
  });
  
  var createTransform = function createTransform(transforms) {
      return function (name) {
          return transforms.reduce(function (name, fn) {
              return name && fn(name);
          }, name);
      };
  };
  
  var rebindAll = (function (target, source) {
      for (var _len = arguments.length, transforms = Array(_len > 2 ? _len - 2 : 0), _key = 2; _key < _len; _key++) {
          transforms[_key - 2] = arguments[_key];
      }
  
      var transform = createTransform(transforms);
      var _iteratorNormalCompletion = true;
      var _didIteratorError = false;
      var _iteratorError = undefined;
  
      try {
          for (var _iterator = Object.keys(source)[Symbol.iterator](), _step; !(_iteratorNormalCompletion = (_step = _iterator.next()).done); _iteratorNormalCompletion = true) {
              var name = _step.value;
  
              var result = transform(name);
              if (result) {
                  target[result] = createReboundMethod(target, source, name);
              }
          }
      } catch (err) {
          _didIteratorError = true;
          _iteratorError = err;
      } finally {
          try {
              if (!_iteratorNormalCompletion && _iterator.return) {
                  _iterator.return();
              }
          } finally {
              if (_didIteratorError) {
                  throw _iteratorError;
              }
          }
      }
  
      return target;
  });
  
  var regexify = (function (strsOrRegexes) {
      return strsOrRegexes.map(function (strOrRegex) {
          return typeof strOrRegex === 'string' ? new RegExp('^' + strOrRegex + '$') : strOrRegex;
      });
  });
  
  var exclude = (function () {
      for (var _len = arguments.length, exclusions = Array(_len), _key = 0; _key < _len; _key++) {
          exclusions[_key] = arguments[_key];
      }
  
      exclusions = regexify(exclusions);
      return function (name) {
          return exclusions.every(function (exclusion) {
              return !exclusion.test(name);
          }) && name;
      };
  });
  
  var include = (function () {
      for (var _len = arguments.length, inclusions = Array(_len), _key = 0; _key < _len; _key++) {
          inclusions[_key] = arguments[_key];
      }
  
      inclusions = regexify(inclusions);
      return function (name) {
          return inclusions.some(function (inclusion) {
              return inclusion.test(name);
          }) && name;
      };
  });
  
  var includeMap = (function (mappings) {
    return function (name) {
      return mappings[name];
    };
  });
  
  var capitalizeFirstLetter = function capitalizeFirstLetter(str) {
    return str[0].toUpperCase() + str.slice(1);
  };
  
  var prefix = (function (prefix) {
    return function (name) {
      return prefix + capitalizeFirstLetter(name);
    };
  });
  
  exports.rebind = rebind;
  exports.rebindAll = rebindAll;
  exports.exclude = exclude;
  exports.include = include;
  exports.includeMap = includeMap;
  exports.prefix = prefix;
  
  Object.defineProperty(exports, '__esModule', { value: true });
  
  })));
  

(function (global, factory) {
  typeof exports === 'object' && typeof module !== 'undefined' ? factory(exports, require('d3-scale'), require('d3fc-rebind'), require('d3-time')) :
  typeof define === 'function' && define.amd ? define(['exports', 'd3-scale', 'd3fc-rebind', 'd3-time'], factory) :
  (factory((global.fc = global.fc || {}),global.d3,global.fc,global.d3));
}(this, (function (exports,d3Scale,d3fcRebind,d3Time) { 'use strict';

var identity = function () {

    var identity = {};

    identity.distance = function (start, end) {
        return end - start;
    };

    identity.offset = function (start, offset) {
        return start instanceof Date ? new Date(start.getTime() + offset) : start + offset;
    };

    identity.clampUp = function (d) {
        return d;
    };

    identity.clampDown = function (d) {
        return d;
    };

    identity.copy = function () {
        return identity;
    };

    return identity;
};

function tickFilter(ticks, discontinuityProvider) {
    var discontinuousTicks = [];
    var _iteratorNormalCompletion = true;
    var _didIteratorError = false;
    var _iteratorError = undefined;

    try {
        for (var _iterator = ticks[Symbol.iterator](), _step; !(_iteratorNormalCompletion = (_step = _iterator.next()).done); _iteratorNormalCompletion = true) {
            var tick = _step.value;

            var up = discontinuityProvider.clampUp(tick);
            var down = discontinuityProvider.clampDown(tick);
            if (up === down) {
                discontinuousTicks.push(up);
            }
        }
    } catch (err) {
        _didIteratorError = true;
        _iteratorError = err;
    } finally {
        try {
            if (!_iteratorNormalCompletion && _iterator.return) {
                _iterator.return();
            }
        } finally {
            if (_didIteratorError) {
                throw _iteratorError;
            }
        }
    }

    return discontinuousTicks;
}

function discontinuous(adaptedScale) {
    var _this = this;

    if (!arguments.length) {
        adaptedScale = d3Scale.scaleIdentity();
    }

    var discontinuityProvider = identity();

    var scale = function scale(value) {
        var domain = adaptedScale.domain();
        var range = adaptedScale.range();

        // The discontinuityProvider is responsible for determine the distance between two points
        // along a scale that has discontinuities (i.e. sections that have been removed).
        // the scale for the given point 'x' is calculated as the ratio of the discontinuous distance
        // over the domain of this axis, versus the discontinuous distance to 'x'
        var totalDomainDistance = discontinuityProvider.distance(domain[0], domain[1]);
        var distanceToX = discontinuityProvider.distance(domain[0], value);
        var ratioToX = distanceToX / totalDomainDistance;
        var scaledByRange = ratioToX * (range[1] - range[0]) + range[0];
        return scaledByRange;
    };

    scale.invert = function (x) {
        var domain = adaptedScale.domain();
        var range = adaptedScale.range();

        var ratioToX = (x - range[0]) / (range[1] - range[0]);
        var totalDomainDistance = discontinuityProvider.distance(domain[0], domain[1]);
        var distanceToX = ratioToX * totalDomainDistance;
        return discontinuityProvider.offset(domain[0], distanceToX);
    };

    scale.domain = function () {
        if (!arguments.length) {
            return adaptedScale.domain();
        }
        var newDomain = arguments.length <= 0 ? undefined : arguments[0];

        // clamp the upper and lower domain values to ensure they
        // do not fall within a discontinuity
        var domainLower = discontinuityProvider.clampUp(newDomain[0]);
        var domainUpper = discontinuityProvider.clampDown(newDomain[1]);
        adaptedScale.domain([domainLower, domainUpper]);
        return scale;
    };

    scale.nice = function () {
        adaptedScale.nice();
        var domain = adaptedScale.domain();
        var domainLower = discontinuityProvider.clampUp(domain[0]);
        var domainUpper = discontinuityProvider.clampDown(domain[1]);
        adaptedScale.domain([domainLower, domainUpper]);
        return scale;
    };

    scale.ticks = function () {
        for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
            args[_key] = arguments[_key];
        }

        var ticks = adaptedScale.ticks.apply(_this, args);
        return tickFilter(ticks, discontinuityProvider);
    };

    scale.copy = function () {
        return discontinuous(adaptedScale.copy()).discontinuityProvider(discontinuityProvider.copy());
    };

    scale.discontinuityProvider = function () {
        if (!arguments.length) {
            return discontinuityProvider;
        }
        discontinuityProvider = arguments.length <= 0 ? undefined : arguments[0];
        return scale;
    };

    d3fcRebind.rebindAll(scale, adaptedScale, d3fcRebind.include('range', 'rangeRound', 'interpolate', 'clamp', 'tickFormat'));

    return scale;
}

var skipWeekends = function () {

    // the indices returned by date.getDay()
    var day = {
        sunday: 0,
        monday: 1,
        saturday: 6
    };

    var millisPerDay = 24 * 3600 * 1000;
    var millisPerWorkWeek = millisPerDay * 5;
    var millisPerWeek = millisPerDay * 7;

    var skipWeekends = {};

    var isWeekend = function isWeekend(date) {
        return date.getDay() === 0 || date.getDay() === 6;
    };

    skipWeekends.clampDown = function (date) {
        if (date && isWeekend(date)) {
            // round the date up to midnight
            var newDate = d3Time.timeDay.ceil(date);
            // then subtract the required number of days
            if (newDate.getDay() === day.sunday) {
                return d3Time.timeDay.offset(newDate, -1);
            } else if (newDate.getDay() === day.monday) {
                return d3Time.timeDay.offset(newDate, -2);
            } else {
                return newDate;
            }
        } else {
            return date;
        }
    };

    skipWeekends.clampUp = function (date) {
        if (date && isWeekend(date)) {
            // round the date down to midnight
            var newDate = d3Time.timeDay.floor(date);
            // then add the required number of days
            if (newDate.getDay() === day.saturday) {
                return d3Time.timeDay.offset(newDate, 2);
            } else if (newDate.getDay() === day.sunday) {
                return d3Time.timeDay.offset(newDate, 1);
            } else {
                return newDate;
            }
        } else {
            return date;
        }
    };

    // returns the number of included milliseconds (i.e. those which do not fall)
    // within discontinuities, along this scale
    skipWeekends.distance = function (startDate, endDate) {
        startDate = skipWeekends.clampUp(startDate);
        endDate = skipWeekends.clampDown(endDate);

        // move the start date to the end of week boundary
        var offsetStart = d3Time.timeSaturday.ceil(startDate);
        if (endDate < offsetStart) {
            return endDate.getTime() - startDate.getTime();
        }

        var msAdded = offsetStart.getTime() - startDate.getTime();

        // move the end date to the end of week boundary
        var offsetEnd = d3Time.timeSaturday.ceil(endDate);
        var msRemoved = offsetEnd.getTime() - endDate.getTime();

        // determine how many weeks there are between these two dates
        // round to account for DST transitions
        var weeks = Math.round((offsetEnd.getTime() - offsetStart.getTime()) / millisPerWeek);

        return weeks * millisPerWorkWeek + msAdded - msRemoved;
    };

    skipWeekends.offset = function (startDate, ms) {
        var date = isWeekend(startDate) ? skipWeekends.clampUp(startDate) : startDate;

        if (ms === 0) {
            return date;
        }

        var isNegativeOffset = ms < 0;
        var isPositiveOffset = ms > 0;
        var remainingms = ms;

        // move to the end of week boundary for a postive offset or to the start of a week for a negative offset
        var weekBoundary = isNegativeOffset ? d3Time.timeMonday.floor(date) : d3Time.timeSaturday.ceil(date);
        remainingms -= weekBoundary.getTime() - date.getTime();

        // if the distance to the boundary is greater than the number of ms
        // simply add the ms to the current date
        if (isNegativeOffset && remainingms > 0 || isPositiveOffset && remainingms < 0) {
            return new Date(date.getTime() + ms);
        }

        // skip the weekend for a positive offset
        date = isNegativeOffset ? weekBoundary : d3Time.timeDay.offset(weekBoundary, 2);

        // add all of the complete weeks to the date
        var completeWeeks = Math.floor(remainingms / millisPerWorkWeek);
        date = d3Time.timeDay.offset(date, completeWeeks * 7);
        remainingms -= completeWeeks * millisPerWorkWeek;

        // add the remaining time
        date = new Date(date.getTime() + remainingms);
        return date;
    };

    skipWeekends.copy = function () {
        return skipWeekends;
    };

    return skipWeekends;
};

var _typeof = typeof Symbol === "function" && typeof Symbol.iterator === "symbol" ? function (obj) {
  return typeof obj;
} : function (obj) {
  return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj;
};





















var get = function get(object, property, receiver) {
  if (object === null) object = Function.prototype;
  var desc = Object.getOwnPropertyDescriptor(object, property);

  if (desc === undefined) {
    var parent = Object.getPrototypeOf(object);

    if (parent === null) {
      return undefined;
    } else {
      return get(parent, property, receiver);
    }
  } else if ("value" in desc) {
    return desc.value;
  } else {
    var getter = desc.get;

    if (getter === undefined) {
      return undefined;
    }

    return getter.call(receiver);
  }
};

















var set = function set(object, property, value, receiver) {
  var desc = Object.getOwnPropertyDescriptor(object, property);

  if (desc === undefined) {
    var parent = Object.getPrototypeOf(object);

    if (parent !== null) {
      set(parent, property, value, receiver);
    }
  } else if ("value" in desc && desc.writable) {
    desc.value = value;
  } else {
    var setter = desc.set;

    if (setter !== undefined) {
      setter.call(receiver, value);
    }
  }

  return value;
};

var provider = function provider() {
    for (var _len = arguments.length, ranges = Array(_len), _key = 0; _key < _len; _key++) {
        ranges[_key] = arguments[_key];
    }

    var inRange = function inRange(number, range) {
        return number > range[0] && number < range[1];
    };

    var surroundsRange = function surroundsRange(inner, outer) {
        return inner[0] >= outer[0] && inner[1] <= outer[1];
    };

    var identity = {};

    identity.distance = function (start, end) {
        start = identity.clampUp(start);
        end = identity.clampDown(end);

        var surroundedRanges = ranges.filter(function (r) {
            return surroundsRange(r, [start, end]);
        });
        var rangeSizes = surroundedRanges.map(function (r) {
            return r[1] - r[0];
        });

        return end - start - rangeSizes.reduce(function (total, current) {
            return total + current;
        }, 0);
    };

    var add = function add(value, offset) {
        return value instanceof Date ? new Date(value.getTime() + offset) : value + offset;
    };

    identity.offset = function (location, offset) {
        if (offset > 0) {
            var _ret = function () {
                var currentLocation = identity.clampUp(location);
                var offsetRemaining = offset;
                while (offsetRemaining > 0) {
                    var futureRanges = ranges.filter(function (r) {
                        return r[0] > currentLocation;
                    }).sort(function (a, b) {
                        return a[0] - b[0];
                    });
                    if (futureRanges.length) {
                        var nextRange = futureRanges[0];
                        var delta = nextRange[0] - currentLocation;
                        if (delta > offsetRemaining) {
                            currentLocation = add(currentLocation, offsetRemaining);
                            offsetRemaining = 0;
                        } else {
                            currentLocation = nextRange[1];
                            offsetRemaining -= delta;
                        }
                    } else {
                        currentLocation = add(currentLocation, offsetRemaining);
                        offsetRemaining = 0;
                    }
                }
                return {
                    v: currentLocation
                };
            }();

            if ((typeof _ret === "undefined" ? "undefined" : _typeof(_ret)) === "object") return _ret.v;
        } else {
            var _ret2 = function () {
                var currentLocation = identity.clampDown(location);
                var offsetRemaining = offset;
                while (offsetRemaining < 0) {
                    var futureRanges = ranges.filter(function (r) {
                        return r[1] < currentLocation;
                    }).sort(function (a, b) {
                        return b[0] - a[0];
                    });
                    if (futureRanges.length) {
                        var nextRange = futureRanges[0];
                        var delta = nextRange[1] - currentLocation;
                        if (delta < offsetRemaining) {
                            currentLocation = add(currentLocation, offsetRemaining);
                            offsetRemaining = 0;
                        } else {
                            currentLocation = nextRange[0];
                            offsetRemaining -= delta;
                        }
                    } else {
                        currentLocation = add(currentLocation, offsetRemaining);
                        offsetRemaining = 0;
                    }
                }
                return {
                    v: currentLocation
                };
            }();

            if ((typeof _ret2 === "undefined" ? "undefined" : _typeof(_ret2)) === "object") return _ret2.v;
        }
    };

    identity.clampUp = function (d) {
        return ranges.reduce(function (value, range) {
            return inRange(value, range) ? range[1] : value;
        }, d);
    };

    identity.clampDown = function (d) {
        return ranges.reduce(function (value, range) {
            return inRange(value, range) ? range[0] : value;
        }, d);
    };

    identity.copy = function () {
        return identity;
    };

    return identity;
};

exports.scaleDiscontinuous = discontinuous;
exports.discontinuitySkipWeekends = skipWeekends;
exports.discontinuityIdentity = identity;
exports.discontinuityRange = provider;

Object.defineProperty(exports, '__esModule', { value: true });

})));
