% BanditDataCleaning
%% clean arrays
% sort arrays
[~,index] = sortrows([v3data.trial].'); v3data = v3data(index); clear index
[~,index] = sortrows([v3data.block].'); v3data = v3data(index); clear index
[~,index] = sortrows([v3data.ID].'); v3data = v3data(index); clear index

[~,index] = sortrows([v4data.trial].'); v4data = v4data(index); clear index
[~,index] = sortrows([v4data.block].'); v4data = v4data(index); clear index
[~,index] = sortrows([v4data.ID].'); v4data = v4data(index); clear index

%purge duplicates from arrays
IDs = [v3data.ID];
blocks = [v3data.block];
trials = [v3data.trial];
startTimes = [v3data.startTime];
points = [v3data.pointsWon];
[~, IA] = unique([IDs; blocks; trials; startTimes; points]', 'rows');
v3data = v3data(IA);

IDs = [v4data.ID];
blocks = [v4data.block];
trials = [v4data.trial];
startTimes = [v4data.startTime];
points = [v4data.pointsWon];
[~, IA] = unique([IDs; blocks; trials; startTimes; points]', 'rows');
v4data = v4data(IA);

%% check data
uniqueVals_v3 = unique([v3data.ID]);
uniqueVals_v4 = unique([v4data.ID]);
for i = 1:numel(uniqueVals_v3)
   nTrials_v3(i) = sum([v3data.ID] == uniqueVals_v3(i));
   firstTrialPresent_v3(i) = sum([v3data.ID] == uniqueVals_v3(i) & [v3data.block] == 1 & [v3data.trial] == 1);
end

for i = 1:numel(uniqueVals_v4)    
   nTrials_v4(i) = sum([v4data.ID] == uniqueVals_v4(i));
   firstTrialPresent_v4(i) = sum([v4data.ID] == uniqueVals_v4(i) & [v4data.block] == 1 & [v4data.trial] == 1);
end


%% interpolate data
cleanable = uniqueVals_v3(nTrials_v3 > 85 & nTrials_v3 < 90);

% v3
v3data([v3data.ID] == 4022748).ID = 40232748;


for i = 1:numel(cleanable)
    
    cleanableID = cleanable(i);
    subsetData = v3data([v3data.ID] == cleanableID);
    
    if isempty(find([subsetData.block] == 1 & [subsetData.trial] == 1))
        continue
    end
    
    payoffs = cat(3,subsetData(1).payoffs{:});
    missingLocs = find(~ismember(diff([subsetData.trial]), [1 -29]));
    for ii = 1:numel(missingLocs)
        missingBlock = subsetData(missingLocs(ii)).block;
        missingTrial = subsetData(missingLocs(ii)).trial + 1;
        if missingTrial == 30
            continue
        end
        
        missingRewardAmt = subsetData(missingLocs(ii) + 1).blockWinnings - subsetData(missingLocs(ii) + 1).pointsWon - subsetData(missingLocs(ii)).blockWinnings;
        missingChoice = find(squeeze(payoffs(missingBlock,missingTrial,:)) == missingRewardAmt);
        
        if numel(missingChoice) ~= 1
            continue
        end
        
        v3data(end+1) = subsetData(1);
        v3data(end).block = missingBlock;
        v3data(end).trial = missingTrial;
        v3data(end).responseTime = NaN;
        v3data(end).pointsWon = missingRewardAmt;
        v3data(end).blockWinnings = subsetData(missingLocs(ii) + 1).blockWinnings - subsetData(missingLocs(ii) + 1).pointsWon;
        v3data(end).whichFilled = subsetData(missingLocs(ii)).whichFilled; 
       
        switch missingChoice
            
            case 1
                v3data(end).choice = 'top';
            case 2
                v3data(end).choice = 'right';
            case 3
                v3data(end).choice = 'bottom';
            case 4
                v3data(end).choice = 'left';
        end
        
        
        
    end
    
    
end

% v4
cleanable = uniqueVals_v4(nTrials_v4 > 85 & nTrials_v4 < 90);
for i = 1:numel(cleanable)
    
    cleanableID = cleanable(i);
    subsetData = v4data([v4data.ID] == cleanableID);
    
    if isempty(find([subsetData.block] == 1 & [subsetData.trial] == 1))
        continue
    end
    
    payoffs = cat(3,subsetData(1).payoffs{:});
    missingLocs = find(~ismember(diff([subsetData.trial]), [1 -29]));
    for ii = 1:numel(missingLocs)
        missingBlock = subsetData(missingLocs(ii)).block;
        missingTrial = subsetData(missingLocs(ii)).trial + 1;
        if missingTrial == 30
            continue
        end
        
        missingRewardAmt = subsetData(missingLocs(ii) + 1).blockWinnings - subsetData(missingLocs(ii) + 1).pointsWon - subsetData(missingLocs(ii)).blockWinnings;
        missingChoice = find(squeeze(payoffs(missingBlock,missingTrial,:)) == missingRewardAmt);
        
        if numel(missingChoice) ~= 1
            continue
        end
        
        v4data(end+1) = subsetData(1);
        v4data(end).block = missingBlock;
        v4data(end).trial = missingTrial;
        v4data(end).responseTime = NaN;
        v4data(end).pointsWon = missingRewardAmt;
        v4data(end).blockWinnings = subsetData(missingLocs(ii) + 1).blockWinnings - subsetData(missingLocs(ii) + 1).pointsWon;
        v4data(end).whichFilled = subsetData(missingLocs(ii)).whichFilled; 
       
        switch missingChoice
            
            case 1
                v4data(end).choice = 'top';
            case 2
                v4data(end).choice = 'right';
            case 3
                v4data(end).choice = 'bottom';
            case 4
                v4data(end).choice = 'left';
        end
        
        
        
    end
    
    
end

% sort arrays
[~,index] = sortrows([v3data.trial].'); v3data = v3data(index); clear index
[~,index] = sortrows([v3data.block].'); v3data = v3data(index); clear index
[~,index] = sortrows([v3data.ID].'); v3data = v3data(index); clear index

[~,index] = sortrows([v4data.trial].'); v4data = v4data(index); clear index
[~,index] = sortrows([v4data.block].'); v4data = v4data(index); clear index
[~,index] = sortrows([v4data.ID].'); v4data = v4data(index); clear index

%% check data
uniqueVals_v3 = unique([v3data.ID]);
uniqueVals_v4 = unique([v4data.ID]);
for i = 1:numel(uniqueVals_v3)
   nTrials_v3(i) = sum([v3data.ID] == uniqueVals_v3(i));
   firstTrialPresent_v3(i) = sum([v3data.ID] == uniqueVals_v3(i) & [v3data.block] == 1 & [v3data.trial] == 1);
end

for i = 1:numel(uniqueVals_v4)    
   nTrials_v4(i) = sum([v4data.ID] == uniqueVals_v4(i));
   firstTrialPresent_v4(i) = sum([v4data.ID] == uniqueVals_v4(i) & [v4data.block] == 1 & [v4data.trial] == 1);
end

nGoodParticipants_v3 = sum(nTrials_v3 == 90);
nGoodParticipants_v4 = sum(nTrials_v4 == 90);
