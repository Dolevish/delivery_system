%% Header file for city_generator records

-record(city, {
    roads = [],           % רשימת כבישים
    houses = [],          % רשימת בתים
    businesses = [],      % רשימת בתי עסק
    intersections = [],   % צמתים
    grid_size = {800, 600} % גודל המפה
}).

-record(road, {
    id,
    type,        % main, secondary, residential
    start_point,
    end_point,
    width
}).

-record(house, {
    id,
    location,
    address,
    size = small  % small, medium, large
}).

-record(business, {
    id,
    location,
    name,
    type,  % restaurant, shop, gas_station, etc.
    size = medium
}).