'use strict';

angular.module("bestHaskellApp", ['ngRoute'])
.config(function($routeProvider){
  $routeProvider
    .when('/', { templateUrl: 'view/index.html'
               , controller:  'IndexController'
               });
})
.directive('rankingTable', function($http){
  return {
    restrict: 'E',
    replace: true,
    templateUrl: 'view/rankingTable.html',
    scope: {caption: '@', route: '@', ranking: '='},
    link: function(scope){
      if(scope.route) {
        $http.get(scope.route).success(function(data){
          scope.items = data.ranking;
        });
      } else {
        scope.$watch('ranking', function(v){
          scope.items = scope.ranking;
        });
      }
    }
  }
})
.controller("IndexController", function($rootScope, $scope, $http){
  $rootScope.title     = "index";
  $http.get('/').success(function(data){
    $scope.nPackages  = data.nPackages;
    $scope.lastUpdate = data.lastUpdate;
    $scope.total      = data.total.ranking;
    $scope.weekly     = data.weekly.ranking;
    $scope.monthly    = data.monthly.ranking;
  });
});
