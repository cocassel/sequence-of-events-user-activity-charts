import csv, datetime, firebase_admin
from firebase_admin import credentials,db
import collections

def auth():
    '''
    Authenitcates user using service account
    '''
    try:
        # Fetch the service account key JSON file contents
        cred = credentials.Certificate('shining-heat-4904-3be4b7446c57.json')
        # Initialize the app with a service account, granting admin privileges
        #The firebase app should only be initialize once
        if (not len(firebase_admin._apps)):
            firebase_admin.initialize_app(cred, {
                'databaseURL': 'https://shining-heat-4904.firebaseio.com/'
            })
    except:
        print("The session key was not found in the repository or an there is an invalid session key.")
        print("Please contact Prof. Schneider if you believe that you should have access to the Macaron data")


def get_data():
    '''
    Returns a dictionary of all the data in the DB organized by collection
    '''
    auth()
    #There are 4 databases in the firebase. Github and test_data are confirmed to be the most trustable sources with asdf_data and 1 being testing db's
    github_data = db.reference('github:704403').get()
    #asdf_data = db.reference('asdf').get()
    #one_data = db.reference('1').get()
    test_data = db.reference('test').get()
    return {'github':github_data,'test':test_data}

def clean_data(nonredundant):
    #boolean called nonrundant passed through.
    # True = sequentially,repeated VTICON_main behaviours removed.
    # False = sequentially,repeated VTICON_main behaviours remain
    '''
    Cleans firebase data into a writeable format & returns list of dicts
    '''
    to_clean = get_data()
    count = 0
    collection_names = list(to_clean.keys())
    # List of all column headers
    field_names = ['collection','session','log_id','animation','interface','start_time','t',
    'value','datetime']
    # List of dicts to be written to .csv
    cleaned_data = []
    #running through each collection (github_data, asdf_data, one_data, test_data)
    for collection in collection_names:
        collection_data = to_clean[collection]


        #get a list of session_Id
        session_ids = list(collection_data.keys())
        for session in session_ids:
            #get dictionary of session info
            session_data = collection_data[session]

            actions = list(session_data.keys())
            animation = session_data['animation']
            interface = session_data['interface']
            start_time = session_data['startTime']
            #checks if there is log information. If there is no log id, then it is not save
            try:
                log_dict = session_data['log']

                # Sort based on timestamps
                log_dict = collections.OrderedDict(sorted(log_dict.items(), key=lambda x: x[1]['t']))

                log_ids = list(log_dict.keys())
                previous_behavior = "First run"
                previous_vticon = "First run"
                previous_example= "First run"
                start_drag = False
                start_play = False
                start_unmuted = False

                for log_id in log_ids:
                    action_dict = log_dict[log_id]
                    # check for poorly formatted action values. Each poorly formatted block starts with "SAVE" or "LOAD". Permission was given to ignore these samples
                    #one xml observed under load

                    if action_dict['value'][0:6] == 'SAVE_{':
                        previous_behavior = 'SAVE_{'
                        continue
                    if action_dict['value'][0:5] == 'LOAD_':
                        previous_behavior = 'LOAD_'
                        continue
                    #aternate behaviors between vitcon_select_main and vticon_select_example
                    if action_dict['value'][0:13] == 'VTICON_SELECT':
                        select_type = action_dict['value'][13:]
                        if select_type == previous_vticon:
                            continue
                        else:
                            previous_vticon = select_type

                    #remove repeating types of example_select_vn. The n type should change eachtype an example_select_vn appears
                    if action_dict['value'][0:14] == 'EXAMPLE_SELECT':
                        example_select_version = action_dict['value'][15:]
                        if example_select_version == previous_example:
                            continue
                        else:
                            previous_example = example_select_version

                    # Only accept an alternating pattern of STARTDRAG and STOPDRAG events
                    if action_dict['value'][0:9] == "STARTDRAG":
                        if start_drag == True:
                            continue
                        else:
                            start_drag = True

                    if action_dict['value'][0:8] == "STOPDRAG":
                        if start_drag == False:
                            continue
                        else:
                            start_drag = False

                    # Only accept an alternating pattern of PLAYBACK_SETPLAY_true and PLAYBACK_SETPLAY_false
                    # (or PLAYBACK_PLAYEND) events
                    if action_dict['value'] == "PLAYBACK_SETPLAY_true":
                        if start_play == True:
                            continue
                        else:
                            start_play = True

                    if action_dict['value'] == "PLAYBACK_SETPLAY_false" or action_dict['value'] == "PLAYBACK_PLAYEND":
                        if start_play == False:
                            continue
                        else:
                            start_play = False

                    # Only accept an alternating pattern of PLAYBACK_MUTE_false and PLAYBACK_MUTE_true events
                    if action_dict['value'] == "PLAYBACK_MUTE_false":
                        if start_unmuted == True:
                            continue
                        else:
                            start_unmuted = True

                    if action_dict['value'] == "PLAYBACK_MUTE_true":
                        if start_unmuted == False:
                            continue
                        else:
                            start_unmuted = False

                    save_dict = {
                        'collection':collection,
                        'session':session,
                        'log_id':log_id,
                        'animation':animation,
                        'interface':interface,
                        'start_time':start_time,
                        't':action_dict['t'],
                        'value':action_dict['value'],
                        'datetime':datetime.datetime.fromtimestamp(action_dict['t']/1000).strftime('%Y-%m-%d %H:%M:%S.%f')
                    }
                    action_value = action_dict['value']
                    cleaned_data.append(dict(save_dict))

                    previous_behavior = action_value
            except:
                #this ignores every session that does not have behavior logs
                count+=1
    return [cleaned_data,field_names]


def save_nonredundant_dict_to_csv():
    nonredundant = True
    return_list = clean_data(nonredundant)
    field_names = return_list[1]
    cleaned_data = return_list[0]
    count = 0
    with open('firebase_data_shining_heat_4904_nonredundant.csv', mode='w') as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=field_names)
        writer.writeheader()
        for row in cleaned_data:
            try:
                writer.writerow(row)
            except:
                count += 1

def main():
    save_nonredundant_dict_to_csv()

if __name__ == "__main__": main()
